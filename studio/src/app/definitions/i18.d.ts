interface I18nCore {
  close: string;
}

interface I18nLinks {
  home: string;
  discover: string;
  enterprise: string;
  about: string;
  team: string;
  newsletter: string;
  contact: string;
  press: string;
  faq: string;
  developers: string;
  open_source: string;
  services: string;
  developer: string;
  terms: string;
  terms_of_use: string;
  privacy_policy: string;
}

interface I18n {
  lang: 'en';
  core: I18nCore;
  links: I18nLinks;
}
