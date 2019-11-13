const express = require('express');
const serveStatic = require('serve-static');

module.exports = () => {
    const app = express();

    app.use(serveStatic('dist', {'index': ['index.html']}));

    const server = app.listen(3003, () => {
        const internalIp = require('internal-ip');

        console.log('\n\x1b[36m%s\x1b[0m', '[DeckDeckGo-Poll]', 'Poll up and running');
        console.log('\x1b[36m%s\x1b[0m', '[DeckDeckGo-Poll]', 'Local:', 'http://localhost:' + server.address().port);
        console.log('\x1b[36m%s\x1b[0m', '[DeckDeckGo-Poll]', 'Remote:', 'http://' + internalIp.v4.sync() + ':' + server.address().port);

        require('./socket-io-poll')(server);
    });
};
