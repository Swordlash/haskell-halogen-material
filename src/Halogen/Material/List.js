import {MDCList} from '@material/list';
import {MDCRipple} from '@material/ripple';

if (!window.Halogen)
  window.Halogen = {};

window.Halogen.init_material_list = function(element) {
  return new MDCList(element);
}

window.Halogen.init_material_list_items = function(mdcList) {
  return mdcList.listElements.map((listItemEl) => new MDCRipple(listItemEl));
}