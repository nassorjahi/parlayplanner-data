$RepoDir = "C:\Users\nasso\OneDrive\Desktop\parlayplanner-data"
Set-Location $RepoDir

git add docs/slate_today.json
git commit -m "Update slate_today.json" | Out-Null
git push
