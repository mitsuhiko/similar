#!/usr/bin/env python3
import json
import os
import re
import subprocess
import sys
from datetime import date, timedelta


UPSTREAM = "mitsuhiko/similar"
UPSTREAM_MD = "UPSTREAM.md"

PRS_HEADING = "## Upstream PRs"
ISSUES_HEADING = "## Issues"


def get_last_tracked_number():
    try:
        with open(UPSTREAM_MD) as f:
            content = f.read()
        numbers = re.findall(
            r"\[#(\d+)\]\(https://github\.com/mitsuhiko/similar", content
        )
        if numbers:
            return max(int(n) for n in numbers)
    except OSError:
        pass
    return None


def gh(*args):
    result = subprocess.run(
        ["gh"] + list(args),
        capture_output=True,
        text=True,
        check=True,
    )
    return result.stdout.strip()


def fetch_items(last_number, since_date):
    merged_prs = json.loads(
        gh(
            "pr", "list",
            "--repo", UPSTREAM,
            "--state", "merged",
            "--limit", "100",
            "--json", "number,title,url",
        )
    )
    open_prs = json.loads(
        gh(
            "pr", "list",
            "--repo", UPSTREAM,
            "--state", "open",
            "--limit", "100",
            "--json", "number,title,createdAt,url",
        )
    )
    open_issues = json.loads(
        gh(
            "issue", "list",
            "--repo", UPSTREAM,
            "--state", "open",
            "--limit", "100",
            "--json", "number,title,createdAt,url",
        )
    )

    if last_number is not None:
        merged_prs = [x for x in merged_prs if x["number"] > last_number]
        open_prs = [x for x in open_prs if x["number"] > last_number]
        open_issues = [x for x in open_issues if x["number"] > last_number]
    else:
        cutoff = since_date.isoformat()
        merged_prs = []
        open_prs = [x for x in open_prs if x.get("createdAt", "") >= cutoff]
        open_issues = [x for x in open_issues if x.get("createdAt", "") >= cutoff]

    merged_prs.sort(key=lambda x: x["number"])
    open_prs.sort(key=lambda x: x["number"])
    open_issues.sort(key=lambda x: x["number"])

    return merged_prs, open_prs, open_issues


def append_to_section(content, heading, new_rows):
    lines = content.split("\n")
    in_section = False
    last_table_row = -1

    for i, line in enumerate(lines):
        if line.startswith("## "):
            in_section = line.strip() == heading
        if in_section and line.startswith("|"):
            last_table_row = i

    if last_table_row == -1:
        return content

    return "\n".join(
        lines[: last_table_row + 1] + new_rows + lines[last_table_row + 1 :]
    )


def main():
    last_number = get_last_tracked_number()
    since_date = date.today() - timedelta(days=30)

    merged_prs, open_prs, open_issues = fetch_items(last_number, since_date)

    total = len(merged_prs) + len(open_prs) + len(open_issues)
    if total == 0:
        print("No new upstream activity.")
        return

    with open(UPSTREAM_MD) as f:
        content = f.read()

    pr_rows = []
    for pr in open_prs:
        pr_rows.append(f"| [#{pr['number']}]({pr['url']}) | {pr['title']} | open |")
    for pr in merged_prs:
        pr_rows.append(
            f"| [#{pr['number']}]({pr['url']}) | {pr['title']} | merged — not cherry-picked |"
        )

    issue_rows = []
    for issue in open_issues:
        issue_rows.append(
            f"| [#{issue['number']}]({issue['url']}) | {issue['title']} | open |"
        )

    if pr_rows:
        content = append_to_section(content, PRS_HEADING, pr_rows)
    if issue_rows:
        content = append_to_section(content, ISSUES_HEADING, issue_rows)

    with open(UPSTREAM_MD, "w") as f:
        f.write(content)

    github_output = os.environ.get("GITHUB_OUTPUT")
    if github_output:
        with open(github_output, "a") as f:
            f.write("changed=true\n")

    print(
        f"Added {len(open_prs)} open PR(s), {len(merged_prs)} merged PR(s), "
        f"{len(open_issues)} issue(s) to {UPSTREAM_MD}"
    )


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as e:
        print(f"gh command failed: {e.stderr}", file=sys.stderr)
        sys.exit(1)
