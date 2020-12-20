require('dotenv').config({
  path: `.env.${process.env.NODE_ENV}`,
});

const fetch = require('node-fetch');

exports.sourceNodes = async ({boundActionCreators}) => {
  const activeEnv = process.env.GATSBY_ACTIVE_ENV || process.env.NODE_ENV || 'development';

  if (activeEnv !== 'production') {
    return;
  }

  if (!process.env.FIREBASE_FUNCTIONS_URL || !process.env.FEED_TOKEN) {
    return;
  }

  try {
    const rawResponse = await fetch(`${process.env.FIREBASE_FUNCTIONS_URL}/feed`, {
      method: 'POST',
      headers: {
        Accept: 'application/json',
        'Content-Type': 'application/json',
        Authorization: `Bearer ${process.env.FEED_TOKEN}`,
      },
    });

    if (!rawResponse || !rawResponse.ok) {
      console.error('Cannot fetch feed data.');
      return;
    }

    const feed = await rawResponse.json();

    const fs = require('fs');
    fs.writeFileSync('./tmp.json', JSON.stringify(feed));
  } catch (err) {
    console.error(err);
  }
};
