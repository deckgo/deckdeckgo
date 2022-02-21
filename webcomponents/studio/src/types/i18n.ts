export interface I18nIndicator {
  saving: string;
  loading: string;
}

export type Languages = 'en' | 'es' | 'de' | 'nl';

export interface I18n {
  lang: Languages;
  indicator: I18nIndicator;
}
