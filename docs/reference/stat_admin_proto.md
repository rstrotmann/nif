# stat_admin_proto ggproto object

A ggplot2 stat for treatment administrations that creates vertical lines
at administration times.

## Details

This stat requires a data frame with columns 'x' and 'admin', where
'admin' should be a logical or numeric column indicating administration
events (1 or TRUE for administrations, 0 or FALSE otherwise).
