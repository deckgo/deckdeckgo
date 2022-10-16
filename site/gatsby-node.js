require('dotenv').config({
  path: `.env.${process.env.NODE_ENV}`
});

const fs = require('fs');
const fetch = require('node-fetch');
const {createRemoteFileNode} = require(`gatsby-source-filesystem`);

const FEED_NODE_TYPE = 'Feed';

exports.onCreateNode = async ({node, actions: {createNode}, createNodeId, getCache}) => {
  if (node.internal.type === FEED_NODE_TYPE) {
    const fileNode = await createRemoteFileNode({
      url: node.data.screenshot,
      parentNodeId: node.id,
      createNode,
      createNodeId,
      getCache
    });
    if (fileNode) {
      node.remoteImage___NODE = fileNode.id;
    }
  }
};

exports.sourceNodes = async ({actions, createNodeId, createContentDigest}) => {
  const feed = JSON.parse(fs.readFileSync('./decks.sample.json'));
  createNodes(actions, createNodeId, createContentDigest, feed);
};

// https://www.gatsbyjs.com/docs/how-to/plugins-and-themes/creating-a-source-plugin/
function createNodes(actions, createNodeId, createContentDigest, feed) {
  const {createNode} = actions;

  feed.forEach((entry) =>
    createNode({
      ...entry,
      id: createNodeId(`${FEED_NODE_TYPE}-${entry.id}`),
      parent: null,
      children: [],
      internal: {
        type: `${FEED_NODE_TYPE}`,
        content: JSON.stringify(entry),
        contentDigest: createContentDigest(entry)
      }
    })
  );
}
