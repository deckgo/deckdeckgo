import {Utils} from '../core/utils';

export interface GoogleFont {
  id: string;
  name: string;
  family: string;
}

export class FontsUtils {
  // prettier-ignore
  static fonts: GoogleFont[] = [
    {
      id: 'google-fonts-lora',
      name: 'Lora',
      family: '\'Lora\', serif'
    },
    {
      id: 'google-fonts-roboto',
      name: 'Roboto',
      family: '\'Roboto\', sans-serif'
    },
    {
      id: 'google-fonts-open-sans',
      name: 'Open Sans',
      family: '\'Open Sans\', sans-serif'
    },
    {
      id: 'google-fonts-montserrat',
      name: 'Montserrat',
      family: '\'Montserrat\', sans-serif'
    },
    {
      id: 'google-fonts-cabin',
      name: 'Cabin',
      family: '\'Cabin\', sans-serif'
    },
    {
      id: 'google-fonts-lato',
      name: 'Lato',
      family: '\'Lato\', sans-serif'
    },
    {
      id: 'google-fonts-muli',
      name: 'Muli',
      family: '\'Muli\', sans-serif'
    },
    {
      id: 'google-fonts-source-sans-pro',
      name: 'Source Sans Pro',
      family: '\'Source Sans Pro\', sans-serif'
    },
    {
      id: 'google-fonts-libre-baskerville',
      name: 'Libre Baskerville',
      family: '\'Libre Baskerville\', serif'
    }
  ];

  static async loadAllGoogleFonts(googleFontsUrl: string) {
    try {
      const promises = this.fonts.map((font: GoogleFont) => {
        return Utils.injectCSS(font.id, googleFontsUrl + font.name.replace(' ', '+'));
      });

      await Promise.all(promises);
    } catch (err) {
      // We ignore this error. Show must go on aka will fallback on default font
    }
  }

  static loadGoogleFont(googleFontsUrl: string, style: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!style || style === undefined || !style['font-family'] || style['font-family'] === undefined) {
        resolve(undefined);
        return;
      }

      const fontFamily: string | undefined = style['font-family'];

      const font: GoogleFont | undefined = await this.extractGoogleFont(fontFamily);

      if (!font || font === undefined) {
        resolve(undefined);
        return;
      }

      await Utils.injectCSS(font.id, this.getGoogleFontUrl(googleFontsUrl, font));

      resolve();
    });
  }

  static getGoogleFontUrl(googleFontsUrl: string, font: GoogleFont): string {
    return googleFontsUrl + font.name.replace(' ', '+');
  }

  static extractGoogleFont(fontFamilyStyle: string): Promise<GoogleFont | undefined> {
    return new Promise<GoogleFont | undefined>(async (resolve) => {
      if (!fontFamilyStyle || fontFamilyStyle === undefined) {
        resolve(undefined);
        return;
      }

      const fontFamily: string = fontFamilyStyle.replace(/\'/g, '').replace(/"/g, '');

      const font: GoogleFont = this.fonts.find((font: GoogleFont) => {
        return fontFamily === font.family.replace(/\'/g, '');
      });

      resolve(font);
    });
  }
}
