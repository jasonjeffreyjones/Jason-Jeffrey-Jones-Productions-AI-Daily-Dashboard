#!/bin/bash
#NOTE: first needed to add RSA key fingerprint for jasodfzw@premium15.web-hosting.com. did this by ssh-ing and manually agreeing.
#note, sshpass needs to be installed for this to work
#have this execute via chrontab once per day in the wee hours.

# Diagnostics to know where this bash script looks for things.
#pwd
#which Rscript
#which python3

# Specify this Rscript: ~/miniconda3/bin/Rscript
Rscript /home/ec2-user/ai_daily/R/wrangle-qualtrics.R
Rscript /home/ec2-user/ai_daily/R/wrangle-prolific-demographics.R
python3 /home/ec2-user/ai_daily/python/ai_daily_upload_data.py

echo "completed wrangle_and_upload.sh"