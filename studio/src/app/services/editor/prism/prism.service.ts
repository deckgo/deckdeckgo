import {AssetsService} from '../../core/assets/assets.service';

export interface PrismLanguage {
  language: string;
  title: string;
}

export class PrismService {
  private languages: PrismLanguage[];

  private static instance: PrismService;

  private assetsService: AssetsService;

  private constructor() {
    this.assetsService = AssetsService.getInstance();
  }

  static getInstance() {
    if (!PrismService.instance) {
      PrismService.instance = new PrismService();
    }
    return PrismService.instance;
  }

  getLanguage(language: string): Promise<PrismLanguage | undefined> {
    return new Promise<PrismLanguage | undefined>(async (resolve) => {
      const allLanguages: PrismLanguage[] = await this.getLanguages();

      if (!allLanguages || allLanguages.length <= 0) {
        resolve(undefined);
        return;
      }

      const result: PrismLanguage = allLanguages.find((filteredLanguage: PrismLanguage) => {
        return filteredLanguage.language.toLowerCase().indexOf(language.toLowerCase()) > -1;
      });

      resolve(result);
    });
  }

  getLanguages(): Promise<PrismLanguage[]> {
    return new Promise<PrismLanguage[]>(async (resolve) => {
      if (this.languages) {
        resolve(this.languages);
        return;
      }

      try {
        const assets: Assets | undefined = await this.assetsService.assets();

        if (assets === undefined || !assets.prism || !assets.prism.definitionSrc) {
          this.initDefaultLanguages();
          resolve(this.languages);
        }

        const response: Response = await fetch(assets.prism.definitionSrc);

        const definition: any = await response.json();

        if (!definition || !definition.languages) {
          this.initDefaultLanguages();
          resolve(this.languages);
          return;
        }

        this.languages = [];

        for (const key in definition.languages) {
          if (definition.languages.hasOwnProperty(key)) {
            const value = definition.languages[key];

            if (value.title && value.title !== '') {
              this.languages.push({
                language: key,
                title: value.title,
              });
            }
          }
        }

        if (this.languages.length <= 0) {
          this.initDefaultLanguages();
        }
      } catch (err) {
        this.initDefaultLanguages();
      }

      resolve(this.languages);
    });
  }

  private initDefaultLanguages() {
    // Per default prismjs supports Javascript without any extra languages definitions needed
    this.languages = [
      {
        language: 'javascript',
        title: 'Javascript',
      },
    ];
  }
}
