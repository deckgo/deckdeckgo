export type Expanded = 'open' | 'close';

export type EditMode = 'standard' | 'css';

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
  edit: EditMode;
}
