let rooms = [];

module.exports = server => {
  const socketIO = require("socket.io").listen(server, {
    transports: ["websocket", "xhr-polling"]
  });

  socketIO.set("origins", "*:*");

  console.log("\x1b[36m%s\x1b[0m", "[DeckDeckGo]", "Socket listening. Path: /");

  socketIO.sockets.on("connection", socket => {
    socket.on("rooms", async () => {
      await emitRooms(socketIO, socket, false);
    });

    socket.on("join", async req => {
      if (req) {
        socket.join(req.room);
        socket.emit("joined");

        if (req.room && req.deck) {
          socket.broadcast.to(req.room).emit("deck_joined");

          if (rooms && rooms.indexOf(req.room) < 0) {
            rooms.push(req.room);
          }

          await emitRooms(socketIO, socket, true);
        }
      }
    });

    socket.on("leave", async req => {
      if (req && req.room) {
        socket.leave(req.room);
      }
    });

    socket.on("signal", req => {
      socket.broadcast.to(req.room).emit("signaling_message", {
        type: req.type,
        message: req.message,
        fromSocketId: socket.id
      });
    });

    socket.on("start", req => {
      if (req && req.toSocketId) {
        socketIO.to(`${req.toSocketId}`).emit("signaling_message", {
          type: req.type,
          message: req.message
        });
      }
    });
  });
};

function filterActiveRooms(socketIO) {
  return new Promise(async resolve => {
    const results = [];

    if (rooms && rooms.length > 0) {
      rooms.forEach(room => {
        const activeRoom = socketIO.sockets.adapter.rooms
          ? socketIO.sockets.adapter.rooms[room]
          : null;

        if (activeRoom) {
          results.push({
            room: room,
            clients: activeRoom.length
          });
        }
      });
    }

    resolve(results);
  });
}

/**
 * activeRooms = [{
 *      room: string, // room name
 *      clients: number // 1 for app or deck, 2 if both are connected
 * }]
 */
function emitRooms(socketIO, socket, broadcast) {
  return new Promise(async resolve => {
    const activeRooms = await filterActiveRooms(socketIO);

    if (broadcast) {
      socket.broadcast.emit("active_rooms", {
        rooms: activeRooms
      });
    } else {
      socket.emit("active_rooms", {
        rooms: activeRooms
      });
    }

    resolve();
  });
}
