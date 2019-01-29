import csv

path = '/network/lustre/iss01/home/maxime.kermarquer/statistics/data/2018/parsable/collapse/'

with open( path + '03_mars.txt', 'r') as csvfile:
	filereader = csv.reader(csvfile, delimiter='|')
	for row in filereader:
	 	print (row)