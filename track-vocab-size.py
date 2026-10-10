from datetime import date
from io import BytesIO

import pandas as pd
import requests
from plotnine import aes, element_text, geom_point, ggplot, scale_x_discrete, theme
from tqdm import tqdm

OWNER = "notkarenyi"
REPO = "chinese-practice"
CSV = "r/vocab.csv"
XLSX = "r/vocab.xlsx"

headers = {"Accept": "application/vnd.github+json"}

rows = (
    pd.read_csv("file_history.csv").to_records()
    if pd.io.common.file_exists("file_history.csv")
    else pd.DataFrame()
)

# Get commits that modified the file
url = f"https://api.github.com/repos/{OWNER}/{REPO}/commits"

rows = []

for file in [CSV, XLSX]:
    params = {"path": file, "per_page": 100}

    commits = requests.get(url, headers=headers, params=params).json()

    for commit in tqdm(commits):
        if not commit in rows:
            sha = commit["sha"]
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
df["day"] = pd.to_datetime(df["date"]).dt.date
df["month"] = pd.to_datetime(df["date"]).dt.to_period("M").astype(str)
df.sort_values(by="month", inplace=True)
month = df.groupby("month")["len"].mean().reset_index()

df.to_csv("file_history.csv", index=False)

(
    ggplot(month, aes(x="month", y="len"))
    + geom_point()
    + scale_x_discrete(
        limits=pd.period_range(
            month["month"].min(), month["month"].max(), freq="M"
        ).astype(str)
    )
    + theme(axis_text_x=element_text(rotation=90, ha="right"))
)
