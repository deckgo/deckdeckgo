import { EnvironmentCloud } from '../types/env';
import envStore from '../stores/env.store';

export const initEnv = async ({cloud}: {cloud: EnvironmentCloud | undefined}) => {
  envStore.state.cloud = {...cloud};
}
