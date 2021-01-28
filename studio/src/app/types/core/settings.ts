export interface SettingsPanels {
  borderRadius: 'open' | 'close';
  boxShadow: 'open' | 'close';
  align: 'open' | 'close';
  fontSize: 'open' | 'close';
  letterSpacing: 'open' | 'close';
  image: 'open' | 'close';
}

export interface Settings {
  panels: SettingsPanels;
}
