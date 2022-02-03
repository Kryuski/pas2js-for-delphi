This directory contains 2 examples for use with Data Abstract.

The sampleda project (and sampleda.html page) connect to the sample Data
Abstract server hosted at Remobjects software. It is read-only.

The sampledarw project (and sampledarw.html page) is read-write.
It connects to a service located at /proxy/Server/Bin (you must change this if
your service is listening somewhere else). 
The server is available in the directory "server", where you will also find a find the SQL needed to create
the table.

To get it to work:

- You need a webserver.  (simpleserver distributed with FPC works fine)

- use country.sql to create the table in the database of your choice.

- Open server in Delphi (you need Data Abstract of course)

- Set the database connection string in the Modules.Server cmConnection component.

- Compile server and start server. By default it listens on port 8099.

- In the sampledarw.lpr, correctly set the URL where the server is
  listening on the connection component, see the CreateDataset method:

  FConn:=TDaConnection.Create(Self);
  FConn.URL:='/proxy/Server/bin';

  If you use apache, you can configure it to forward the url to the data abstract application server:

  <Location "/proxy/Server/">
     ProxyPass "http://127.0.0.1:8099/"
  </Location>
 
  if you use simpleserver, create a simpleserver.ini config file:

[Server]
; Correct these
Port=6789
Directory=/home/michael/pas2js/demo/dataabstract

[Proxy]
Server=http://127.0.0.1:8099/

  in either case, you can leave the URL as is.

- compile the lazarus project, and visit the sampledarw.html page in the browser.




