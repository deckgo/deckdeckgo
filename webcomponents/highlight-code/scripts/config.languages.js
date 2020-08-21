#!/usr/bin/env node

const fs = require('fs');
const fetch = require('node-fetch');

const langInterfaces = `export interface DeckdeckgoHighlightCodeLanguageAlias {
  [index: string]: string;
}

export interface DeckdeckgoHighlightCodeLanguage {
  title: string;
  require?: string[];
}

export interface DeckdeckgoHighlightCodeLanguages {
  [index: string]: DeckdeckgoHighlightCodeLanguage;
}`;

async function getLanguages() {
  const response = await fetch('https://raw.githubusercontent.com/PrismJS/prism/master/components.json');

  if (!response || !response.ok) {
    throw new Error('Prismjs configuration cannot be fetched');
  }

  const content = await response.json();

  return content.languages;
}

(async () => {
  try {
    const languages = await getLanguages();

    if (!languages || languages.length <= 0) {
      throw new Error('No languages found in configuration.');
    }

    const filteredLanguages = {};
    Object.keys(languages)
      .filter((key) => key !== 'meta')
      .forEach((key) => {
        filteredLanguages[key] = {
          title: languages[key].title,
        };

        if (languages[key].require && Array.isArray(languages[key].require) && languages[key].require.length > 0) {
          filteredLanguages[key].require = languages[key].require.filter((req) => req !== 'clike' && req !== 'javascript');
        } else if (languages[key].require && languages[key].require !== 'clike' && languages[key].require !== 'javascript') {
          filteredLanguages[key].require = [languages[key].require];
        }
      });

    const languagesEnum = `${langInterfaces}

    export const deckdeckgoHighlightCodeLanguages: DeckdeckgoHighlightCodeLanguages = ${JSON.stringify(filteredLanguages)}`;

    fs.writeFile(`./src/declarations/deckdeckgo-highlight-code-languages.tsx`, languagesEnum, 'utf8', (err) => {
      if (err) return console.log(err);
    });
  } catch (e) {
    console.error(e);
  }
})();
