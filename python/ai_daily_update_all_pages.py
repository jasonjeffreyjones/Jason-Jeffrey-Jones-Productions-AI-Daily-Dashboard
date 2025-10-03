#!/usr/bin/python3

# Updates each HTML page in pageList.
# Put this script on a server, and run it daily using cron.

import datetime
import json
import re
import subprocess
import os
from dotenv import load_dotenv

def main():
	pageList = ["index", "ai-support-trend", "ai-polarization", "research-questions", "about", "overall-predict-risk", "overall-predict-age", "overall-predict-trust", "overall-predict-sex", "americans-say-about-ai"]
	
	# There will be three loops through pageList.
	# The first executes each R script to create the updated dictionaries.
	# The second uses the HTML templates to write out new HTML pages (locally) with data from the dictionaries.
	# The third uses ssh to overwrite the live HTML pages with the newly updated local versions.
	
	# The first loop executes each R script to create updated dictionaries.
	for thisPage in pageList:
		# Command to run dictionary-making R scripts with Rscript.
		command = [f"Rscript /home/ec2-user/ai_daily/R/create-{thisPage}-dictionary.R"]
		
		try:
			result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
			print("Rscript output:", result.stdout.strip())
		except subprocess.CalledProcessError as e:
			print("Attempted:", command)
			print("Failed with error:", e.stderr.strip())
	
	# The second loop uses the HTML templates to write out new HTML pages (locally) with data from the dictionaries.
	for thisPage in pageList:
		# Define file paths
		input_file_path = f'/home/ec2-user/ai_daily/templates-html/template-{thisPage}.html'
		dictionary_file_path = f'/home/ec2-user/ai_daily/json/{thisPage}.json'
		output_file_path = f'/home/ec2-user/ai_daily/{thisPage}.html'

		# Read the input file
		with open(input_file_path, 'r') as file:
			content = file.read()

		# Read dictionary_file_path into a dictionary.
		key_value_pairs = {}
		with open(dictionary_file_path, "r") as file:
			key_value_pairs = json.load(file)
		
		# Convert values to their first element if they are lists
		# R saved each value as a list (enclosed in square brackets in the json)
		for key, value in key_value_pairs.items():
			if isinstance(value, list) and len(value) == 1:
				key_value_pairs[key] = value[0]
		
		# Replace the text
		for theKey in key_value_pairs:
			#content = content.replace(theKey, key_value_pairs[theKey])
			findTheKeyPattern = "\\b" + theKey + "\\b"
			findTheKeyPattern = re.compile(findTheKeyPattern)
			content = re.sub(findTheKeyPattern, str(key_value_pairs[theKey]), content)
		
		# Get the current date
		current_date = datetime.date.today()
		
		# Date in YYYY-MM-DD format
		current_date = current_date.strftime('%Y-%m-%d')
		
		# TODAYS_DATE_PYTHON is not currently in use.
		content = content.replace('TODAYS_DATE_PYTHON', current_date)
		
		# Write the updated content to the output file
		with open(output_file_path, 'w') as file:
			file.write(content)
		
		print(f"{current_date} updated {thisPage}.html completed by {__file__}")
	
	# Before the final loop, overwrite all images to recent versions.
	# Pull the password from a local hidden file.
	password = ""
	#with open("/home/ec2-user/ai_daily/python/.ninjacreds", 'r') as file:
	#	password = file.read().strip()
	load_dotenv()  # grab environment variables from .env file
	password = os.getenv("NINJA_PASSWORD")
	username = os.getenv("NINJA_USERNAME")
	
	command = [f"sshpass -p '{password}' scp -P 21098 /home/ec2-user/ai_daily/images/*.* {username}@premium15.web-hosting.com:/home/{username}/public_html/social-science-dashboard-inator/jjjp-ai-daily-dashboard/images/"]
	try:
		result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
		print("Images overwritten")
	except subprocess.CalledProcessError as e:
		print("Images overwrite failed with error:", e.stderr.strip())

	# The third loop uses ssh to overwrite the live HTML pages with the newly updated local versions.
	for thisPage in pageList:
		# SSH to overwrite each page.
		command = [f"sshpass -p '{password}' scp -O -P 21098 /home/ec2-user/ai_daily/{thisPage}.html {username}@premium15.web-hosting.com:/home/{username}/public_html/social-science-dashboard-inator/jjjp-ai-daily-dashboard/{thisPage}.html"]
		
		try:
			result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
			print(f"{thisPage} HTML page overwritten")
		except subprocess.CalledProcessError as e:
			print(f"{thisPage} HTML page overwrite failed with error:", e.stderr.strip())
	
		

if __name__ == "__main__":
        main()



