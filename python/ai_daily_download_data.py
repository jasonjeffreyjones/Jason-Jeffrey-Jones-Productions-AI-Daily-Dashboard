#!/usr/bin/python3

# Uses the Qualtrics API to download the cumulative data file of responses.
# Put this script on a server, and run it daily using cron.

import requests
import time
import sys
import datetime
import os
from dotenv import load_dotenv

def main():
	load_dotenv()  # grab environment variables from .env file
	
	# Set up your base URL and API Token and surveyID
	my_data_center = os.getenv("QUALTRICS_DATA_CENTER")
	base_url = os.getenv("QUALTRICS_BASE_URL")
	api_token = os.getenv("QUALTRICS_API_TOKEN")
	survey_id = os.getenv("QUALTRICS_SURVEY_ID")
	
	# Headers with API token for authentication
	headers = {
		'X-API-TOKEN': api_token,
		'Content-Type': 'application/json'
	}
	
	payload = {
		'surveyId': survey_id,
		'format': 'csv',  # The format of the export (e.g., 'csv', 'json', 'spss')
		'useLabels': True  # Ensures that response text is used instead of numeric codes
	}
	
	# 1. Create an export job
	create_response = requests.post(base_url, headers=headers, json=payload)
	if 'error' in create_response:
		print("Error:", create_response['error']['errorMessage'])
		sys.exit(1)
	
	export_job_id = create_response.json()['result']['id']
	
	# 2. Check the export job status
	while True:
		status_response = requests.get(f"{base_url}/{export_job_id}", headers=headers)
		status = status_response.json()['result']['status']
		if status == 'complete':
			break
		time.sleep(10)  # Wait for some time before checking again
	
	# Get the current date
	current_date = datetime.date.today()
	
	# Date in YYYY-MM-DD format
	current_date = current_date.strftime('%Y-%m-%d')
	
	# 3. Download the export
	download_response = requests.get(f"{base_url}/{export_job_id}/file", headers=headers, stream=True)
	with open(f'/home/ec2-user/ai_daily/data/qualtrics-download-{current_date}.zip', 'wb') as f:
		for chunk in download_response.iter_content(chunk_size=8192):
			f.write(chunk)
	
	print(f"{current_date} download responses from Qualtrics completed by {__file__}")


if __name__ == "__main__":
	main()



