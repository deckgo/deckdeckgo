const express = require('express');
const serveStatic = require('serve-static');

const app = express();

app.use(serveStatic('dist', {'index': ['index.html']}));

app.get('/googlefonts/', (req, res) => {
    res.json({
        key: '{{KEY}}'
    });
});

const server = app.listen(3002, () => {
    const internalIp = require('internal-ip');

    console.log('\x1b[36m%s\x1b[0m', '[DeckDeckGo]', 'Server up and running');
    console.log('\x1b[36m%s\x1b[0m', '[DeckDeckGo]', 'Local:', 'http://localhost:' + server.address().port);
    console.log('\x1b[36m%s\x1b[0m', '[DeckDeckGo]', 'Remote:', 'http://' + internalIp.v4.sync() + ':' + server.address().port);

    require('./socket-io')(server);
});



