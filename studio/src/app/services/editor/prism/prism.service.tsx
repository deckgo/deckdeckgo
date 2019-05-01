import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

export interface PrismLanguage {
    language: string;
    title: string;
}

export class PrismService {

    private languages: PrismLanguage[];

    private static instance: PrismService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!PrismService.instance) {
            PrismService.instance = new PrismService();
        }
        return PrismService.instance;
    }

    getLanguages(): Promise<PrismLanguage[]> {
        return new Promise<PrismLanguage[]>(async (resolve) => {
           if (this.languages) {
               resolve(this.languages);
               return;
           }

           try {
               const definitionUrl: string = EnvironmentConfigService.getInstance().get('prismComponentsUrl');

               if (!definitionUrl) {
                   this.initDefaultLanguages();
                   resolve(this.languages);
                   return;
               }

               const response: Response = await fetch(definitionUrl);

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
                               title: value.title
                           })
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
        this.languages = [{
            language: 'javascript',
            title: 'Javascript'
        }];
    }
}
