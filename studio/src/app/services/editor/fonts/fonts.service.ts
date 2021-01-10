import {Utils} from '../../../utils/core/utils';

import {EnvironmentGoogleConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {AssetsService} from '../../core/assets/assets.service';

export class FontsService {
  private static instance: FontsService;

  private assetsService: AssetsService;

  private constructor() {
    this.assetsService = AssetsService.getInstance();
  }

  static getInstance() {
    if (!FontsService.instance) {
      FontsService.instance = new FontsService();
    }
    return FontsService.instance;
  }

  async loadAllGoogleFonts(): Promise<GoogleFont[] | undefined> {
    try {
      const assets: Assets = await this.assetsService.assets();

      if (!assets || !assets.fonts || assets.fonts.length <= 0) {
        return undefined;
      }

      const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');

      const promises = assets.fonts.map((font: GoogleFont) => {
        return Utils.injectCSS(font.id, google.fontsUrl + font.name.replace(' ', '+'));
      });

      await Promise.all(promises);

      return assets.fonts;
    } catch (err) {
      // We ignore this error. Show must go on aka will fallback on default font
    }
  }

  loadGoogleFont(googleFontsUrl: string, style: string): Promise<void> {
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

      const assets: Assets = await this.assetsService.assets();

      if (!assets || !assets.fonts || assets.fonts.length <= 0) {
        resolve(undefined);
        return;
      }

      const fontFamily: string = fontFamilyStyle.replace(/\'/g, '').replace(/"/g, '');

      const font: GoogleFont = assets.fonts.find((font: GoogleFont) => {
        return fontFamily === font.family.replace(/\'/g, '');
      });

      resolve(font);
    });
  }
}
