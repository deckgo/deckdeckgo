export type Expanded = 'open' | 'close';

export type EditMode = 'properties' | 'css';

export interface SettingsPanels {
  borderRadius: Expanded;
  boxShadow: Expanded;
  align: Expanded;
  fontSize: Expanded;
  letterSpacing: Expanded;
  image: Expanded;
  color: Expanded;
  background: Expanded;
  list: Expanded;
}

export interface Settings {
  panels: SettingsPanels;
  editMode: EditMode;
}
