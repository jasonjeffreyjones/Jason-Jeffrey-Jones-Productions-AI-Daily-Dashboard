#!/usr/bin/python3

# SFTPs the most recent data file to jjjp-ai-support-dail.csv on ninja.
# Also uses the Open Science Framework API to upload the cumulative canonical data file.
# Cumulative canonical data file contains all responses with joined demographics. No personally identifying information.
# Put this script on a server, and run it daily using cron.

import os
import requests
import json
import time
import sys
import datetime
import subprocess
from dotenv import load_dotenv

def main():
	# Get the current date
	current_date = datetime.date.today()
	
	# Date in YYYY-MM-DD format
	current_date = current_date.strftime('%Y-%m-%d')
	
	# Path to the CSV file you want to upload
	csv_file_path = f'/home/ec2-user/ai_daily/data/jjjp-ai-support-daily-{current_date}.csv'
	
	load_dotenv()  # grab environment variables from .env file
	password = os.getenv("NINJA_PASSWORD")
	username = os.getenv("NINJA_USERNAME")
	
	# Take the most recent csv file from here on AWS and scp it to data directory on ninja.
	command = [f"sshpass -p '{password}' scp -O -P 21098 {csv_file_path} {username}@premium15.web-hosting.com:/home/{username}/public_html/social-science-dashboard-inator/jjjp-ai-daily-dashboard/data/jjjp-ai-support-daily.csv"]
	try:
		result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
		print(f"{csv_file_path} sent to ninja")
	except subprocess.CalledProcessError as e:
		print(f"{csv_file_path} send failed with error:", e.stderr.strip())
	
	# Ninja save complete.  Now move on to OSF backup.
	osf_token = os.getenv("OSF_API_TOKEN")
	
	# Define the URL to get the folder details
	project_id = '2ndsf'
	folder_name = "ai-support"
	folder_url = f"https://api.osf.io/v2/nodes/{project_id}/files/osfstorage/"
	
	# Retrieve the folder metadata to get the upload link
	response = requests.get(folder_url)
	if response.status_code != 200:
		raise Exception(f"Failed to retrieve folder info: {response.status_code}")
	
	folder_data = response.json()['data']
	ai_support_folder = next((item for item in folder_data if item['attributes']['name'] == folder_name), None)
	
	if not ai_support_folder:
		raise Exception("The 'ai-support' folder was not found in OSF storage.")
	
	upload_url = ai_support_folder['links']['upload']
	
	# Prepare the headers with your OSF token
	headers = {
		'Authorization': f'Bearer {osf_token}',
		#'Content-Type': 'application/json',
	}

	file_name = os.path.basename(csv_file_path)
	
	# Define the upload parameters
	params = {
		"kind": "file",
		"name": file_name,
	}
	
	# Read the file data and upload it
	with open(csv_file_path, 'rb') as file_data:
		response = requests.put(upload_url, headers=headers, params=params, data=file_data)
	
	# Check the response status
	if response.status_code == 201:
		print(f"{current_date} upload canonical cumulative file completed by {__file__}")
		#print("File uploaded successfully!")
		#print("Response:", response.json())
	elif response.status_code == 409:
		print("On OSF, a file with the same name already exists.")
	else:
		print(f"Failed to upload the file. Status code: {response.status_code}")
		print("Error response:", response.json())


if __name__ == "__main__":
        main()







