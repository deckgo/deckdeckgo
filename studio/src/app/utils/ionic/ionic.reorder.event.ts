import type { ItemReorderEventDetail } from '@ionic/core';

// Interface so that ItemReorderEventDetail is not imported without "type" in component.d.ts
export interface IonicReorderEvent extends ItemReorderEventDetail {}
