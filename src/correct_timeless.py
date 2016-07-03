#! /usr/bin/python

import csv
import re
import pprint
import copy
import datetime
import locale

input_fn = "malformed.csv"
output_fn = "malformed_for_sql.csv" 

def main():
  input_f = open("./"+input_fn,'rb')
  output_f = open("./"+output_fn, 'w+')
  reader = csv.reader(input_f,delimiter=',')
  
  ok_rows = 0
  ko_rows = 0
  
  for i,row in enumerate(reader):
    new_row = get_new_row(row)
    if is_ok_row(new_row):
      ok_rows += 1
      output_f.write((",".join(new_row))+"\n")
    else:
      ko_rows += 1
    
  input_f.close()
  output_f.close()
  
  print("OK rows:"+str(ok_rows))
  print("KO rows:"+str(ko_rows))
  
def get_new_row(row):
  return row[:1]+[change_datetime(field) for field in row[1:]]   

def change_datetime(dt_str):
  try:
    dt = datetime.datetime.strptime(dt_str,"%d/%m/%Y %H:%M:%S")
    #dt = datetime.datetime.strptime(dt_str,"%x %X")
    return dt.isoformat(' ')
  except ValueError:
    try:
      dt = datetime.datetime.strptime(dt_str,"%d/%m/%Y")
      return dt.isoformat(' ')
    except ValueError:
      return False


def is_ok_row(row):
  return not (False in row[1:])

main()