export interface SettingsPanels {
  borderRadius: 'open' | 'close';
  boxShadow: 'open' | 'close';
  align: 'open' | 'close';
  fontSize: 'open' | 'close';
  letterSpacing: 'open' | 'close';
  image: 'open' | 'close';
  color: 'open' | 'close';
  background: 'open' | 'close';
}

export interface Settings {
  panels: SettingsPanels;
}
