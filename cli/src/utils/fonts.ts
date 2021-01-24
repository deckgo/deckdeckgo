import fs from 'fs';

import {Spinner} from 'cli-spinner';
import {bold} from 'colorette';
import {createDir, downloadFont, downloadFontsKey, downloadFontsList, downloadToFile} from './download';

interface FontAnswer {
  font: boolean;
}

interface FontNameAnswer {
  fontName: string;
}

interface FontVariantsAnswer {
  fontVariants: string[];
}

interface FontSubsetsAnswer {
  fontSubsets: string[];
}

interface FontSelection {
  font?: Font;
  searchAgain?: boolean;
}

interface FontCss {
  eot: string;
  woff2: string;
  woff: string;
  ttf: string;
  svg: string;
}

interface Font {
  kind: string;
  family: string;
  category: string;
  variants: string[];
  subsets: string[];
  version: string;
  lastModified: string;
  files: any;
}

interface FontsList {
  items: Font[];
}

interface FontKey {
  key: string;
}

export function installFont(folder: string) {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const inquirerFonts = require('inquirer');

      const questionFont = [
        {
          type: 'confirm',
          name: 'font',
          message: 'Do you want to use a Google Font for your presentation?',
          default: false
        }
      ];

      const answer: FontAnswer = await inquirerFonts.prompt(questionFont);

      if (answer.font) {
        const fontsList: FontsList = await fetchFontsList();

        if (fontsList && fontsList.items && fontsList.items.length > 0) {
          const font: Font = await selectFont(fontsList);
          if (font) {
            const fontSubsets: string[] = await selectFontSubsets(font);

            const fontVariants: string[] = await selectFontVariants(font);

            const fontCss: FontCss = await downloadFontAllCSS(font, fontVariants, fontSubsets);

            const localFontCss: FontCss = await downloadInstallFont(folder + '/src/assets/fonts/', fontCss);

            await writeCssFiles(folder, font, localFontCss);
          }
        }
      }

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function writeCssFiles(folder: string, font: Font, fontCss: FontCss) {
  return new Promise<void>(async (resolve, reject) => {
    if (!folder || !font || !font.family || !fontCss) {
      resolve();
      return;
    }

    const loading = new Spinner(bold('Writing to CSS files...'));
    loading.setSpinnerString(18);
    loading.start();

    try {
      await createFontCssFile(folder + '/src/css/google-fonts.css', font.family, fontCss);

      await addLoadFontCssFile(folder + '/src/index.js');

      loading.stop(true);

      resolve();
    } catch (err) {
      loading.stop(true);
      reject(err);
    }
  });
}

function addLoadFontCssFile(filePath: string) {
  return new Promise<void>(async (resolve, reject) => {
    try {
      fs.appendFile(filePath, "\n\nimport './css/google-fonts.css';", (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function createFontCssFile(filePath: string, family: string, fontCss: FontCss) {
  return new Promise<void>(async (resolve, reject) => {
    try {
      let content = '';

      if (fontCss.eot) {
        content += fontCss.eot;
      }

      if (fontCss.woff2) {
        content += fontCss.woff2;
      }

      if (fontCss.woff) {
        content += fontCss.woff;
      }

      if (fontCss.ttf) {
        content += fontCss.ttf;
      }

      if (fontCss.svg) {
        content += fontCss.svg;
      }

      if (!content || content.length === 0) {
        resolve();
        return;
      }

      content += getCssCode(family);

      fs.writeFile(filePath, content, (err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      });
    } catch (err) {
      reject(err);
    }
  });
}

function getCssCode(family: string) {
  let code = '\n:root {\n';
  code += `--ion-font-family: "${family}";\n`;
  code += '}\n\n* {\n';
  code += `font-family: "${family}";\n`;
  code += `--ion-font-family: "${family}";\n`;
  code += '}\n';

  return code;
}

function downloadFontAllCSS(font: Font, fontSubsets: string[], fontVariants: string[]) {
  return new Promise<FontCss>(async (resolve, reject) => {
    const loading = new Spinner(bold('Downloading font...'));
    loading.setSpinnerString(18);
    loading.start();

    try {
      if (font && fontSubsets && fontSubsets.length > 0 && fontVariants && fontVariants.length > 0) {
        const promises = [];

        promises.push(downloadFontCSS(font.family, fontVariants, fontSubsets, 'Mozilla/5.0 (Windows; U; MSIE 7.0; Windows NT 6.0; en-US)'));
        promises.push(
          downloadFontCSS(
            font.family,
            fontVariants,
            fontSubsets,
            'Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36'
          )
        );
        promises.push(downloadFontCSS(font.family, fontVariants, fontSubsets, 'Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)'));
        promises.push(
          downloadFontCSS(
            font.family,
            fontVariants,
            fontSubsets,
            'Mozilla/5.0 (Macintosh; U; PPC Mac OS X 10_5_2; en-gb) AppleWebKit/526+ (KHTML, like Gecko) Version/3.1 iPhone'
          )
        );
        promises.push(
          downloadFontCSS(
            font.family,
            fontVariants,
            fontSubsets,
            'Mozilla/5.0 (iPhone; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1A543 Safari/419.3'
          )
        );

        const results: string[] = await Promise.all(promises);

        if (results && results.length > 0) {
          resolve({
            eot: results[0],
            woff2: results.length > 1 ? results[1] : null,
            woff: results.length > 2 ? results[2] : null,
            ttf: results.length > 3 ? results[3] : null,
            svg: results.length > 4 ? results[4] : null
          });
        } else {
          resolve(null);
        }
      } else {
        console.log('No font to fetch.\n');
      }

      loading.stop(true);

      resolve(null);
    } catch (err) {
      loading.stop(true);
      console.error(err);
      reject(err);
    }
  });
}

function downloadFontCSS(fontFamily: string, fontSubsets: string[], fontVariants: string[], userAgent: string) {
  return new Promise<string>(async (resolve, reject) => {
    try {
      let fontCss: string = null;

      const buffer: Buffer = await downloadFont(fontFamily, fontVariants, fontSubsets, userAgent);
      if (buffer) {
        fontCss = buffer.toString('utf8');
      }

      resolve(fontCss);
    } catch (err) {
      reject(err);
    }
  });
}

function downloadInstallFont(folder: string, fontCss: FontCss) {
  return new Promise<FontCss>(async (resolve, reject) => {
    if (!folder || !fontCss) {
      resolve(null);
      return;
    }

    if (!fontCss.eot && !fontCss.woff2 && !fontCss.woff && !fontCss.ttf && !fontCss.svg) {
      resolve(null);
      return;
    }

    const loading = new Spinner(bold('Downloading font...'));
    loading.setSpinnerString(18);
    loading.start();

    try {
      await createDir(folder);

      const promises = [];

      promises.push(downloadInstallFontType(folder, fontCss.eot));
      promises.push(downloadInstallFontType(folder, fontCss.woff2));
      promises.push(downloadInstallFontType(folder, fontCss.woff));
      promises.push(downloadInstallFontType(folder, fontCss.ttf));
      promises.push(downloadInstallFontType(folder, fontCss.svg));

      const results: string[] = await Promise.all(promises);

      if (results && results.length > 0) {
        resolve({
          eot: results[0],
          woff2: results.length > 1 ? results[1] : null,
          woff: results.length > 2 ? results[2] : null,
          ttf: results.length > 3 ? results[3] : null,
          svg: results.length > 4 ? results[4] : null
        });
      } else {
        resolve(null);
      }

      loading.stop(true);

      resolve(fontCss);
    } catch (err) {
      loading.stop(true);
      console.error(err);
      reject(err);
    }
  });
}

function downloadInstallFontType(folder: string, fontTypeCss: string) {
  return new Promise<string>(async (resolve, reject) => {
    if (!fontTypeCss || fontTypeCss.length <= 0) {
      resolve(null);
      return;
    }

    try {
      let result: string = fontTypeCss;

      const extractURL = /(url\()(.*?)(\))/gm;
      let m;
      while ((m = extractURL.exec(fontTypeCss))) {
        if (m && m.length >= 2) {
          const cssToReplace: string = m[0];
          const url: string = m[2];

          const extractFileName = /(?:.+\/)(.+)/gm;
          const match: RegExpExecArray = extractFileName.exec(url);

          if (match && match.length >= 1) {
            let fileName: string = match[1];

            const extractFileNameOld = /(kit=)(.*?)(&)/gm;
            const matchOld: RegExpExecArray = extractFileNameOld.exec(url);
            if (matchOld && matchOld.length >= 2) {
              fileName = `${matchOld[2]}.svg`;
            }

            await downloadToFile(url, folder, fileName);

            result = result.replace(cssToReplace, `url(/assets/fonts/${fileName})`);
          }
        }
      }

      resolve(result);
    } catch (err) {
      reject(err);
    }
  });
}

function selectFontSubsets(font: Font) {
  return new Promise<string[]>(async (resolve, reject) => {
    try {
      if (!font || !font.subsets || font.subsets.length <= 0) {
        resolve(undefined);
        return;
      }

      if (font.subsets.length === 1) {
        resolve(font.subsets);
        return;
      }

      const inquirerFonts = require('inquirer');

      const questionFont = [
        {
          type: 'checkbox',
          name: 'fontSubsets',
          message: 'Select charsets',
          choices: font.subsets,
          default: ['latin']
        }
      ];

      const answer: FontSubsetsAnswer = await inquirerFonts.prompt(questionFont);

      resolve(answer.fontSubsets);
    } catch (err) {
      reject(err);
    }
  });
}

function selectFontVariants(font: Font) {
  return new Promise<string[]>(async (resolve, reject) => {
    try {
      if (!font || !font.variants || font.variants.length <= 0) {
        resolve(undefined);
        return;
      }

      const inquirerFonts = require('inquirer');

      const questionFont = [
        {
          type: 'checkbox',
          name: 'fontVariants',
          message: 'Select styles',
          choices: font.variants,
          default: ['regular']
        }
      ];

      const answer: FontVariantsAnswer = await inquirerFonts.prompt(questionFont);

      resolve(answer.fontVariants);
    } catch (err) {
      reject(err);
    }
  });
}

function selectFont(fontsList: FontsList) {
  return new Promise<Font>(async (resolve, reject) => {
    try {
      let search = true;
      let font: Font;

      while (search) {
        const filteredFonts: Font[] = await filterFonts(fontsList);

        const fontSelection: FontSelection = await chooseFont(filteredFonts);

        font = fontSelection.font;
        search = fontSelection.searchAgain;
      }

      resolve(font);
    } catch (err) {
      reject(err);
    }
  });
}

function chooseFont(filteredFonts: Font[]) {
  return new Promise<FontSelection>(async (resolve, reject) => {
    try {
      const inquirerFonts = require('inquirer');

      let names: string[];

      if (filteredFonts && filteredFonts.length > 0) {
        names = filteredFonts.map((font: Font) => {
          return font.family;
        });
      }

      if (!names || names.length <= 0) {
        names = [];
      }

      const msg: string = names.length > 0 ? 'Select the font' : 'No font found';

      names.push('Search again', 'Skip');

      const questionFont = [
        {
          type: 'list',
          name: 'fontName',
          message: msg,
          choices: names
        }
      ];

      const answer: FontNameAnswer = await inquirerFonts.prompt(questionFont);

      if (answer && answer.fontName === 'Search again') {
        resolve({
          searchAgain: true
        });
      } else if (answer && answer.fontName === 'Skip') {
        resolve({
          searchAgain: false
        });
      } else if (filteredFonts && filteredFonts.length > 0) {
        const findFont: Font = filteredFonts.find((filteredFont: Font) => {
          return filteredFont.family && filteredFont.family.toLowerCase().indexOf(answer.fontName.toLowerCase()) > -1;
        });

        resolve({
          searchAgain: false,
          font: findFont
        });
      } else {
        resolve({
          searchAgain: false
        });
      }
    } catch (err) {
      reject(err);
    }
  });
}

function filterFonts(fontsList: FontsList) {
  return new Promise<Font[]>(async (resolve, reject) => {
    try {
      const inquirerFonts = require('inquirer');

      const questionFont = [
        {
          type: 'input',
          name: 'fontName',
          message: 'Search a Google font (min. 3 characters)?',
          validate: (input: string) => {
            if (input && input.length >= 3) {
              return true;
            } else {
              return "Please provide at least 3 characters for the font's family name";
            }
          }
        }
      ];

      const answer: FontNameAnswer = await inquirerFonts.prompt(questionFont);

      const filteredFonts: Font[] = fontsList.items.filter(
        (filteredFont: Font) => filteredFont.family && filteredFont.family.toLowerCase().indexOf(answer.fontName.toLowerCase()) > -1
      );

      resolve(filteredFonts);
    } catch (err) {
      reject(err);
    }
  });
}

function fetchFontsList() {
  return new Promise<FontsList>(async (resolve, reject) => {
    const loading = new Spinner(bold('Fetching fonts list...'));
    loading.setSpinnerString(18);
    loading.start();

    try {
      let fontsList: FontsList = null;

      const key: string = await fetchFontsKey();

      const buffer: Buffer = await downloadFontsList(key);
      if (buffer) {
        fontsList = JSON.parse(buffer.toString('utf8'));
      }

      loading.stop(true);

      resolve(fontsList);
    } catch (err) {
      loading.stop(true);
      reject(err);
    }
  });
}

function fetchFontsKey() {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const buffer: Buffer = await downloadFontsKey();
      if (buffer) {
        const fontKey: FontKey = JSON.parse(buffer.toString('utf8'));

        if (fontKey && fontKey.key && fontKey.key.length > 0) {
          resolve(fontKey.key);
          return;
        }
      }

      reject('No Google Fonts key found');
    } catch (err) {
      reject(err);
    }
  });
}
