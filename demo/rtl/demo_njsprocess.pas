program demo_njsprocess;

uses
  SysUtils, JS, NodeJS;

//procedure List(s: jsvalue); assembler;
//asm
//  for (var key in s) if (s.hasOwnProperty(key)) console.log('prop="'+key+'"');
//end;

var
  i: Integer;
begin
  //List(TNJSProcess);
  writeln('argv: ',TNJSProcess.argv);
  for i:=0 to length(TNJSProcess.argv)-1 do
    writeln(i,'/',length(TNJSProcess.argv),' ',TNJSProcess.argv[i]);
  writeln('arch=',TNJSProcess.arch);
  writeln('config=',TNJSProcess.config);
  writeln('cwd=',TNJSProcess.cwd);
  writeln('env=',TNJSProcess.env);
  writeln('execArgv=',TNJSProcess.execArgv);
  writeln('execPath=',TNJSProcess.execPath);
  writeln('getegid=',TNJSProcess.getegid);
  writeln('geteuid=',TNJSProcess.geteuid);
  writeln('getgid=',TNJSProcess.getgid);
  writeln('getgroups=',TNJSProcess.getgroups);
  writeln('getuid=',TNJSProcess.getuid);
  writeln('memoryUsage=',TNJSProcess.memoryUsage);
  writeln('pid=',TNJSProcess.pid);
  writeln('platform=',TNJSProcess.platform);
  writeln('release=',TNJSProcess.release);
  writeln('title=',TNJSProcess.title);
  writeln('umask=',TNJSProcess.umask);
  writeln('uptime=',TNJSProcess.uptime);
  writeln('version=',TNJSProcess.version);
  writeln('versions=',TNJSProcess.versions);
end.

