import {FunctionalComponent, h} from '@stencil/core';

import {EnvironmentDeckDeckGoConfig} from '../../../config/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

interface AppIconProps {
  name: string;
  path?: string;
  ariaLabel: string;
  ariaHidden?: boolean;
  slot?: string;
  lazy?: boolean;
  style?: Record<string, string | undefined>;
}

export const AppIcon: FunctionalComponent<AppIconProps> = ({
  name,
  ariaLabel,
  ariaHidden = false,
  path = 'icons/ionicons',
  lazy = false,
  slot,
  style
}: AppIconProps) => {
  const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  return (
    <ion-icon
      src={`${config.globalAssetsUrl}/${path}/${name}.svg`}
      aria-label={ariaLabel}
      aria-hidden={ariaHidden}
      lazy={lazy}
      {...(slot && {slot})}
      {...(style && {style})}
    ></ion-icon>
  );
};
