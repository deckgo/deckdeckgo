import assetsStore from '../../../stores/assets.store';

import {Utils} from '../../../utils/core/utils';

import {EnvironmentGoogleConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../environment/environment-config.service';

export class FontsService {
  private static instance: FontsService;

  static getInstance() {
    if (!FontsService.instance) {
      FontsService.instance = new FontsService();
    }
    return FontsService.instance;
  }

  async loadAllGoogleFonts(): Promise<GoogleFont[] | undefined> {
    try {
      const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');

      const promises = assetsStore.state.fonts.map((font: GoogleFont) => {
        return Utils.injectCSS(font.id, google.fontsUrl + font.name.replace(' ', '+'));
      });

      await Promise.all(promises);

      return assetsStore.state.fonts;
    } catch (err) {
      // We ignore this error. Show must go on aka will fallback on default font
    }
  }

  loadGoogleFont(googleFontsUrl: string, style: Record<string, string> | undefined): Promise<void> {
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

  getGoogleFontUrl(googleFontsUrl: string, font: GoogleFont): string {
    return googleFontsUrl + font.name.replace(' ', '+');
  }

  extractGoogleFont(fontFamilyStyle: string): Promise<GoogleFont | undefined> {
    return new Promise<GoogleFont | undefined>(async (resolve) => {
      if (!fontFamilyStyle || fontFamilyStyle === undefined) {
        resolve(undefined);
        return;
      }

      const fontFamily: string = fontFamilyStyle.replace(/\'/g, '').replace(/"/g, '');

      const font: GoogleFont = assetsStore.state.fonts.find((font: GoogleFont) => {
        return fontFamily === font.family.replace(/\'/g, '');
      });

      resolve(font);
    });
  }
}
