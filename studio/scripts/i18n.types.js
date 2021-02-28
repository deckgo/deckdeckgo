const {writeFileSync, readFileSync} = require('fs');

const prettier = require('prettier');

const generate = async () => {
  const buffer = readFileSync('./src/assets/i18n/en.json');
  const i18n = JSON.parse(buffer.toString());

  const data = Object.keys(i18n).map((key) => {
    const properties = Object.keys(i18n[key]).map((prop) => `${prop}: string;`);

    return {
      key,
      name: `I18n${key.charAt(0).toUpperCase()}${key.slice(1)}`,
      properties,
    };
  });

  const lang = `lang: 'en';`;

  const main = `\n\ninterface I18n {${lang}${data.map((i) => `${i.key}: ${i.name};`).join('')}}`;
  const interfaces = data.map((i) => `\n\ninterface ${i.name} {${i.properties.join('')}}`).join('');

  const output = prettier.format(`${interfaces}${main}`, {semi: false, parser: 'babel', singleQuote: true});

  writeFileSync('./src/app/definitions/i18.d.ts', output);
};

(async () => {
  try {
    await generate();

    console.log(`i18n type declarations generated!`);
  } catch (err) {
    console.error(`Error while generating i18n types.`, err);
  }
})();
