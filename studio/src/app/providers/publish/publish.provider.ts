import {Meta} from '@deckdeckgo/editor';
import {publish as publishProvider, PublishInputs, publishUrl as publishUrlProvider} from '@deckdeckgo/sync';
import {EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const publish = ({inputs, github}: {inputs: PublishInputs, github: boolean}): Promise<void> => {
  const config: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');
  return publishProvider({inputs, config: {...config, github}});
};

export const publishUrl = async (meta: Meta | undefined): Promise<string> => {
  const url: string = await publishUrlProvider(meta);

  if (!url) {
    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return deckDeckGoConfig.website;
  }

  return url;
};
