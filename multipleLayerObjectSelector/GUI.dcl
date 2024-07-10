multipleLayerSelector : dialog {
    label = "Multiple Layer Selector";
    : column {
        : row {
            : list_box {
                label = "Layers:";
                key = "layerList";
                height = 10;
                width = 20;
            }
        }
        : row {
            : button {
                key = "okButton";
                label = "OK";
                is_default = true;
            }
            : button {
                key = "cancelButton";
                label = "Cancel";
                is_cancel = true;
            }
        }
    }
}
