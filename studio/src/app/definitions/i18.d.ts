interface I18nCore {
  close: string;
}

interface I18nNav {
  menu: string;
  sign_out: string;
  sign_in: string;
  write_a_presentation: string;
  ready_to_share: string;
}

interface I18nMenu {
  dashboard: string;
  poll: string;
  remote_control: string;
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

interface I18nOffline {
  editing: string;
}

interface I18nShare {
  a_presentation: string;
  a_presentation_by: string;
  a_presentation_no_author: string;
}

interface I18n {
  lang: 'en';
  core: I18nCore;
  nav: I18nNav;
  menu: I18nMenu;
  links: I18nLinks;
  offline: I18nOffline;
  share: I18nShare;
}
