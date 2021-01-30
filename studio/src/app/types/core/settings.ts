export type Expanded = 'open' | 'close';

export type EditMode = 'properties' | 'css';

export interface SettingsPanels {
  borderRadius: Expanded;
  boxShadow: Expanded;
  block: Expanded;
  fontSize: Expanded;
  text: Expanded;
  image: Expanded;
  color: Expanded;
  background: Expanded;
  list: Expanded;
}

export interface Settings {
  panels: SettingsPanels;
  editMode: EditMode;
}
