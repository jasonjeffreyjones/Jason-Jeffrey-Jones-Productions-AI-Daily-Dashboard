#!/usr/bin/python3

# Uses the Prolific API to download demographics of completed respondents.
# Put this script on a server, and run it daily using cron.

import requests
import time
import datetime
import os
from dotenv import load_dotenv

def download_prolific_demographics(api_token, study_id):
	# URL endpoint for studies
	url = f'https://api.prolific.com/api/v1/studies/{study_id}/export/'
	
	# Header including the Authorization token
	headers = {
		'Authorization': f'Token {api_token}',
		'Content-Type': 'application/json'
	}
	
	# Get the current date
	current_date = datetime.date.today()
	
	# Date in YYYY-MM-DD format
	current_date = current_date.strftime('%Y-%m-%d')
	
	# GET the study demographics with a request
	response = requests.get(url, headers=headers)
	
	# Check if the request was successful
	if response.status_code == 200:
		#print(response)
		#print(response.text)
		#return True
		# The value in response.text is in csv format and contains demographics.
		with open(f'/home/ec2-user/ai_daily/data/prolific-demographics-download-{current_date}.csv', 'w') as f:
			f.write(response.text)
		print(f"{current_date} download demographics from Prolific completed by {__file__}")
	else:
		# HTTP status codes other than 200.
		print(f"{__file__} encountered failed request on {current_date}. Status code: {response.status_code}")
		print("Response:", response.text)
		return None

def main():
	load_dotenv()  # grab environment variables from .env file
	api_token = os.getenv("PROLIFIC_API_TOKEN")
	study_id = os.getenv("PROLIFIC_SURVEY_ID")

	download_prolific_demographics(api_token, study_id)

if __name__ == "__main__":
	main()



