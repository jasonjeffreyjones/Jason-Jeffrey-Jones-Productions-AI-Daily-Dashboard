#!/usr/bin/python3

# Uses the Prolific API to increase places on an existing survey.
# Put this script on a server, and run it daily using cron.

import requests
import time
import datetime
import os
from dotenv import load_dotenv

def increase_study_spaces(api_token, study_id, increase_spaces):
	# URL endpoint for studies
	url = f'https://api.prolific.com/api/v1/studies/{study_id}'
	
	# Header including the Authorization token
	headers = {
		'Authorization': f'Token {api_token}',
		'Content-Type': 'application/json'
	}
	
	# GET the study with a request
	response = requests.get(url, headers=headers)
	
	# Check if the request was successful
	if response.status_code == 200:
		# Parse JSON response
		study = response.json()
		#print(study)
		current_places = study["total_available_places"]
		#print(current_places)
		new_total_places = int(current_places) + increase_spaces
		#new_total_places = str(new_total_places)
		# Sleep a few seconds to avoid hitting API too fast.
		time.sleep(5)
		# JSON payload to update the study
		payload = {
			'total_available_places': f'{new_total_places}'
		}
		# Perform the PATCH request to update the study
		patch_response = requests.patch(url, json=payload, headers=headers)
		# Check if the update was successful
		if patch_response.status_code == 200:
			# Get the current date
			current_date = datetime.date.today()
			# Date in YYYY-MM-DD format
			current_date = current_date.strftime('%Y-%m-%d')
			print(f"{current_date} update Prolific spaces to {new_total_places} completed by {__file__}")
			return True
		else:
			print(f"Failed to update the study. Status code: {response.status_code}")
			print("Response:", response.text)
			return None
	else:
		# Handle errors (you might want to handle different status codes differently)
		print(f"{__file__} failed Prolific API request for study info. Status code: {response.status_code}")
		print("Response:", response.text)
		return None

def main():
	load_dotenv()  # grab environment variables from .env file
	api_token = os.getenv("PROLIFIC_API_TOKEN")
	study_id = os.getenv("PROLIFIC_SURVEY_ID")
	increase_spaces = 11

	increase_study_spaces(api_token, study_id, increase_spaces)

if __name__ == "__main__":
        main()



