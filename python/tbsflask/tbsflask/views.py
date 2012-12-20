from flask import render_template, request, redirect, url_for, jsonify
from tbsflask import app

@app.route('/')
def show_home():
   return render_template('home.html')

