interface I18nCore {
  close: string;
  free_open_source: string;
  loading: string;
}

interface I18nNav {
  menu: string;
  sign_out: string;
  sign_in: string;
  write_a_presentation: string;
  ready_to_share: string;
  profile: string;
  customization: string;
  templates: string;
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

interface I18nShare {
  a_presentation: string;
  a_presentation_by: string;
  a_presentation_no_author: string;
  update_share: string;
  embed: string;
  link: string;
}

interface I18nSign_in {
  hi: string;
  why: string;
  additionally: string;
}

interface I18nSettings {
  un_publish: string;
}

interface I18nDashboard {
  welcome: string;
  your_presentations: string;
  try: string;
  filter: string;
  no_slides: string;
  copy: string;
  delete: string;
}

interface I18nEditor {
  add_slide: string;
  help: string;
  share: string;
  previous: string;
  next: string;
  slides: string;
  style: string;
  present: string;
  go_online: string;
  go_offline: string;
  more: string;
  exit_fullscreen: string;
  backup: string;
  delete: string;
  notes: string;
  copy: string;
  format: string;
  options: string;
  transform: string;
  add_text: string;
  add_shape: string;
  add_image: string;
  image: string;
  qr_code: string;
  chart: string;
  text: string;
  header_footer: string;
  background: string;
  code: string;
  transition: string;
  sides: string;
  huge_title: string;
  large_title: string;
  small_title: string;
  paragraph: string;
  skip: string;
  list: string;
  markdown: string;
  math: string;
  word_cloud: string;
}

interface I18nOffline {
  editing: string;
  why: string;
  turn: string;
  offline: string;
  still_edit: string;
  edit: string;
  offline_now: string;
  hang_on_gather: string;
  oopsie: string;
  check: string;
  cool: string;
  online: string;
  note: string;
  replace: string;
  long_story: string;
  online_now: string;
  hang_on_upload: string;
  error_offline: string;
  error_online: string;
}

interface I18n {
  lang: 'en';
  core: I18nCore;
  nav: I18nNav;
  menu: I18nMenu;
  links: I18nLinks;
  share: I18nShare;
  sign_in: I18nSign_in;
  settings: I18nSettings;
  dashboard: I18nDashboard;
  editor: I18nEditor;
  offline: I18nOffline;
}
