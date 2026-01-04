$RepoDir = "C:\Users\nasso\OneDrive\Desktop\parlayplanner-data"
Set-Location $RepoDir

# Check for changes
$status = git status --porcelain
if ($status) {
    git add docs/slate_today.json
    git commit -m "Update slate_today.json $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
    git push
    Write-Host "✅ Successfully pushed update to GitHub." -ForegroundColor Green
} else {
    Write-Host "ℹ️ No changes detected in JSON file." -ForegroundColor Yellow
}