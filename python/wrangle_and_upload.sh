#!/bin/bash
#NOTE: first needed to add RSA key fingerprint for jasodfzw@premium15.web-hosting.com. did this by ssh-ing and manually agreeing.
#note, sshpass needs to be installed for this to work
#have this execute via chrontab once per day in the wee hours.

# Diagnostics to know where this bash script looks for things.
#pwd
#which Rscript
#which python3

Rscript $HOME/ai-daily/R/wrangle-qualtrics.R
Rscript $HOME/ai-daily/R/wrangle-prolific-demographics.R
"$HOME/ai-daily/.venv/bin/python" "$HOME/ai-daily/python/ai_daily_upload_data.py"

echo "completed wrangle_and_upload.sh"
