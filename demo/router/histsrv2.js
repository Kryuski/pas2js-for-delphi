const http = require('http')
const fs = require('fs')
const httpPort = 3000

http.createServer((req, res) => {
//    console.log(req);
    console.log('Serving file.','.'+req.url);
    var fn =  fs.existsSync('.'+req.url) ? '.'+req.url : "demorouter2.html";
    console.log('Actually Serving file.',fn);
    fs.readFile(fn, 'utf-8', (err, content) => {
    if (err) {
      console.log('We cannot open "'+fn+'" file.')
    }

    res.writeHead(200, {
      'Content-Type': 'text/html; charset=utf-8'
    })

    res.end(content)
  })
}).listen(httpPort, () => {
  console.log('Server listening on: http://localhost:%s', httpPort)
})