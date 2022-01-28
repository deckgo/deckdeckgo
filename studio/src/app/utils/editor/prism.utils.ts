import {DeckdeckgoHighlightCodeLanguage, deckdeckgoHighlightCodeLanguages} from '@deckdeckgo/highlight-code';

import {PrismLanguage} from '../../types/editor/prism-language';

export const filterCodeLanguages = (filter: string | undefined): PrismLanguage[] => {
  const languages: PrismLanguage[] = [];

  for (const key in deckdeckgoHighlightCodeLanguages) {
    if (deckdeckgoHighlightCodeLanguages.hasOwnProperty(key) && (!filter || key.toLowerCase().indexOf(filter.toLowerCase()) > -1)) {
      const value: DeckdeckgoHighlightCodeLanguage = deckdeckgoHighlightCodeLanguages[key];

      if (value.title && value.title !== '') {
        languages.push({
          language: key,
          title: value.title
        });
      }
    }
  }

  return languages;
};

export const getCodeLanguage = async (language: string): Promise<PrismLanguage | undefined> => {
  if (deckdeckgoHighlightCodeLanguages.hasOwnProperty(language)) {
    const value: DeckdeckgoHighlightCodeLanguage = deckdeckgoHighlightCodeLanguages[language];

    return {
      language,
      title: value.title
    };
  }

  return undefined;
};
