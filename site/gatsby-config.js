module.exports = {
  siteMetadata: {
    title: `DeckDeckGo`,
    description: `Make more than presentations. Create, present and share apps. Interact with your audience. With DeckDeckGo, edit your slides anywhere, display them everywhere.`,
    author: `DeckDeckGo`,
    url: `https://deckdeckgo.com`,
    image: '/assets/meta/deckdeckgo-meta.png',
    twitterUsername: '@deckdeckgo',
    siteUrl: `https://deckdeckgo.com`,
    languages: {
      langs: ['en', 'fr'],
      defaultLangKey: 'en',
    },
  },
  plugins: [
    {
      resolve: `gatsby-plugin-sass`,
      options: {
        implementation: require('sass'),
      },
    },
    'gatsby-plugin-sharp',
    'gatsby-plugin-react-helmet',
    'gatsby-plugin-sitemap',
    'gatsby-plugin-offline',
    'gatsby-transformer-sharp',
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'images',
        path: './src/assets/images/',
      },
      __key: 'images',
    },
    {
      resolve: 'gatsby-plugin-i18n',
      options: {
        langKeyDefault: 'en',
        useLangKeyLayout: true,
        prefixDefault: true,
      },
    },
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: `deckdeckgo.com`,
        short_name: `DeckDeckGo`,
        start_url: `/`,
        background_color: `#3a81fe`,
        theme_color: `#ffffff`,
        display: `standalone`,
        icon: `static/assets/deckdeckgo.png`, // This path is relative to the root of the site.
      },
    },
    `gatsby-plugin-robots-txt`,
  ],
};
