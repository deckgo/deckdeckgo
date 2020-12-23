#!/usr/bin/env node

const fs = require('fs');
const fetch = require('node-fetch');

const langInterfaces = `export interface DeckdeckgoHighlightCodeLanguageAlias {
  [index: string]: string;
}

export interface DeckdeckgoHighlightCodeLanguage {
  title: string;
  require?: string[];
  main?:string
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
function getLanguageRequire(language) {
  if (language.require && Array.isArray(language.require) && language.require.length > 0) {
    return language.require.filter((req) => req !== 'clike' && req !== 'javascript');
  } else if (language.require && language.require !== 'clike' && language.require !== 'javascript') {
    return [language.require];
  }
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

        if (languages[key].alias && Array.isArray(languages[key].alias) && languages[key].alias.length > 0) {
          languages[key].alias.forEach((alias) => {
            filteredLanguages[alias] = {
              title: (languages[key].aliasTitles && languages[key].aliasTitles[alias]) || alias,
              main: key,
              require: getLanguageRequire(languages[key]),
            };
          });
        } else if (languages[key].alias && languages[key].alias.length > 0) {
          filteredLanguages[languages[key].alias] = {
            title: (languages[key].aliasTitles && languages[key].aliasTitles[languages[key].alias]) || languages[key].alias,
            main: key,
            require: getLanguageRequire(languages[key]),
          };
        }
        filteredLanguages[key].require = getLanguageRequire(languages[key]);
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
