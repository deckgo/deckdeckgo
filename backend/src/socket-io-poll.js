let polls = [];

module.exports = (server) => {
    const socketIO = require('socket.io').listen(server, {'transports': ['websocket', 'xhr-polling'], path: '/poll'});

    socketIO.set('origins', '*:*');

    console.log('\x1b[36m%s\x1b[0m', '[DeckDeckGo]', 'Socket listening. Path: /poll');

    socketIO.sockets.on('connection', (socket) => {
        socket.on('poll', async (req) => {
            if (req && req.poll) {
                const key = await generateUniqueAvailableKey(0);

                if (key >= 0) {
                    socket.join(key);

                    await addOrUpdatePoll(key, req.poll);

                    socket.emit('poll_key', key);
                }
            }
        });

        socket.on('join', async (req) => {
            if (req && req.key) {
                socket.join(req.key);

                const poll = await findPoll(req.key);

                socket.emit('poll_desc', poll);
            }
        });

        socket.on('update', async (req) => {
            if (req && req.key && req.poll) {
                await addOrUpdatePoll(req.key, req.poll);

                socket.broadcast.to(req.key).emit('poll_updated', req.poll);
            }
        });

        socket.on('vote', async (req) => {
            if (req && req.key && req.answer) {
                const poll = await findPoll(req.key);

                if (poll) {
                    socket.broadcast.to(req.key).emit('vote', req.answer);
                }
            }
        });

        socket.on('leave', async (req) => {
            if (req && req.key) {
                socket.leave(req.key);
            }
        });
    });
};

function addOrUpdatePoll(key, poll) {
    return new Promise(async (resolve) => {
        if (!polls || polls.length <= 0) {
            polls.push({
                key: key,
                poll: poll
            });

            resolve();
            return;
        }

        const index = polls.findIndex((filteredPoll) => {
            return filteredPoll.key === key;
        });

        if (index === -1) {
            polls.push({
                key: key,
                poll: poll
            });
        } else {
            polls[index].poll = poll;
        }

        resolve();
    });
}

function generateUniqueAvailableKey(loop) {
    return new Promise( async (resolve) => {
        let key = `${Math.floor(100000 + Math.random() * 900000)}`;

        if (!polls) {
            resolve(key);
            return;
        }

        const index = polls.findIndex((filteredPoll) => {
            return filteredPoll.key === key;
        });

        if (index > -1) {
            // Avoid loop without end
            if (loop >= (900000 - 100000)) {
                resolve(undefined);
                return;
            }

            key = await generateUniqueAvailableKey(loop + 1);
        }

        resolve(key);
    });
}


function findPoll(key) {
    return new Promise(async (resolve) => {
        if (!polls) {
            resolve(undefined);
            return;
        }

        const poll = polls.find((filteredPoll) => {
            return filteredPoll.key === key;
        });

        resolve(poll);
    });
}
