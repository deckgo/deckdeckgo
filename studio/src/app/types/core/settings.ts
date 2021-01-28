export type Expanded = 'open' | 'close';

export interface SettingsPanels {
  borderRadius: Expanded;
  boxShadow: Expanded;
  align: Expanded;
  fontSize: Expanded;
  letterSpacing: Expanded;
  image: Expanded;
  color: Expanded;
  background: Expanded;
}

export interface Settings {
  panels: SettingsPanels;
}
