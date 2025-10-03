#!/bin/bash
#NOTE: first needed to add RSA key fingerprint for jasodfzw@premium15.web-hosting.com. did this by ssh-ing and manually agreeing.
#note, sshpass needs to be installed for this to work
#have this execute via chrontab once per day in the wee hours.

python3 /home/ec2-user/ai_daily/python/ai_daily_update_all_pages.py
echo "completed update_all_pages.sh"
#need to make executable once on server





