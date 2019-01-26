# README #

This directory contains a pas2js front-end for the WebDemo  demo of FPReport.
It uses Bootstrap CSS to style the application.

To run it, first compile and run the webdemo project.
This is a demo project in the FPC repository in directory
packages/fcl-report/demos

It can be run on your local machine, or on another machine.
By default, it will listen to port 8080 of the machine it runs on;
but you can configure it to run on another port.

Then, in the frmmain.pp look for the BASEURL constant.
This is set up for webdemo running on the local machine, port 8080:

    http://localhost:8080/

Compile the reportdemo project with pas2js and open the reportdemo.html file.
You can select the demo and the output format. The 'Show Report' button will then open the generated report in a new window or in the current window, depending on options.

NOTE:
The demo fetches the list of available report demos through an AJAX call.
if webdemo is running on another machine (or in another domain) then the CORS protection will kick in and the call will fail. If that happens, you can either

* Install a browser plugin to disable CORS (such as CorsE in firefox)
* Put the HTML page on the same machine as where webdemo is running,
  in a directory that can be reached by a webserver such as Apache.
  and change the baseURL to http://:8080/
* As a third altentave, the webdemo accepts a '-s' or '--start' option to
specify a filename which will be served (and all files in the directory) to
serve as an alternate start location

for example:

    home:~/fpc/packages/fcl-report/demos> ./webdemo -s /home/michael/source/pas2js/demo/fpreport/reportdemo.html
    Point your browser to http://localhost:8080/Page  or http://localhost:8080
    An alternate start location is available at http://localhost:8080/Start/reportdemo.html

If you want to run webdemo on another machine in this manner,
then the BaseURL must be changed to / in the frmmain.pas unit
of the reportdemo.
