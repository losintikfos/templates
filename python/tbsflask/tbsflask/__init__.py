from flask import Flask

# create the application
app = Flask(__name__)

# perform any further application init here

# import the application views
import tbsflask.views
