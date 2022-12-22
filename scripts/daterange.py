#https://stackoverflow.com/questions/1060279/iterating-through-a-range-of-dates-in-python
import datetime

def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days)):
        yield start_date + datetime.timedelta(n)