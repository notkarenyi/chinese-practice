"""
Get the history of the vocab file in a GitHub repository and track the number of lines in the file over time. The script fetches commits that modified the specified file, retrieves the content of the file at each commit, and calculates the number of lines. It then saves this information to a JSON file to create a plot showing the average number of lines per month.
"""

from datetime import date
from io import BytesIO

import pandas as pd
import requests
from tqdm import tqdm

OWNER = "notkarenyi"
REPO = "chinese-practice"
CSV = "r/vocab.csv"
XLSX = "r/vocab.xlsx"

headers = {"Accept": "application/vnd.github+json"}

rows = (
    pd.read_json("file_history.json").to_dict("records")
    if pd.io.common.file_exists("file_history.json")
    else []
)
existing_commits = [row["commit"] for row in rows] if len(rows) > 0 else set()

# Get commits that modified the file
url = f"https://api.github.com/repos/{OWNER}/{REPO}/commits"

for file in [CSV, XLSX]:
    params = {"path": file, "per_page": 100}

    commits = requests.get(url, headers=headers, params=params).json()

    for commit in tqdm(commits):
        sha = commit["sha"]
        if sha not in existing_commits:
            date = commit["commit"]["author"]["date"]

            # Get the file at this commit
            file_url = f"https://raw.githubusercontent.com/{OWNER}/{REPO}/{sha}/{file}"

            response = requests.get(file_url)

            if response.ok:
                if file.endswith(".xlsx"):
                    content = pd.read_excel(
                        BytesIO(response.content), header=None
                    ).to_csv(index=False, header=False)
                else:
                    content = response.text

                rows.append({"commit": sha, "date": date, "content": content})

df = pd.DataFrame(rows)
df["content"] = df["content"].str.replace(r"\d+,", "", regex=True)
df["len"] = df["content"].apply(lambda x: len(x.split("\n")))
df["day"] = pd.to_datetime(df["date"], utc=True).dt.date
df["month"] = pd.to_datetime(df["date"], utc=True).dt.to_period("M").astype(str)
df.sort_values(by="month", inplace=True)
month = df.groupby("month")["len"].mean().reset_index()

df.to_json("file_history.json", orient="records", force_ascii=False, indent=2)
