require('dotenv').config({
  path: `.env.${process.env.NODE_ENV}`,
});

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
      getCache,
    });
    if (fileNode) {
      node.remoteImage___NODE = fileNode.id;
    }
  }
};

exports.sourceNodes = async ({boundActionCreators, createNodeId, createContentDigest}) => {
  const activeEnv = process.env.GATSBY_ACTIVE_ENV || process.env.NODE_ENV || 'development';

  const {createNode} = boundActionCreators;

  const fs = require('fs');
  const feed = JSON.parse(fs.readFileSync('./tmp.json'));

  // https://www.gatsbyjs.com/docs/how-to/plugins-and-themes/creating-a-source-plugin/
  feed.forEach((entry) =>
    createNode({
      ...entry,
      id: createNodeId(`${FEED_NODE_TYPE}-${entry.id}`),
      parent: null,
      children: [],
      internal: {
        type: `${FEED_NODE_TYPE}`,
        content: JSON.stringify(entry),
        contentDigest: createContentDigest(entry),
      },
    })
  );

  // if (activeEnv !== 'production') {
  //   return;
  // }
  //
  // if (!process.env.FIREBASE_FUNCTIONS_URL || !process.env.FEED_TOKEN) {
  //   return;
  // }
  //
  // try {
  //   const rawResponse = await fetch(`${process.env.FIREBASE_FUNCTIONS_URL}/feed`, {
  //     method: 'POST',
  //     headers: {
  //       Accept: 'application/json',
  //       'Content-Type': 'application/json',
  //       Authorization: `Bearer ${process.env.FEED_TOKEN}`,
  //     },
  //   });
  //
  //   if (!rawResponse || !rawResponse.ok) {
  //     console.error('Cannot fetch feed data.');
  //     return;
  //   }
  //
  //   const feed = await rawResponse.json();
  //
  //   const fs = require('fs');
  //   fs.writeFileSync('./tmp.json', JSON.stringify(feed));
  // } catch (err) {
  //   console.error(err);
  // }
};
