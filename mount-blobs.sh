#instructions from https://docs.microsoft.com/en-us/azure/storage/blobs/storage-how-to-mount-container-linux

sudo mkdir /mnt/blobfusetmp
sudo chown mattcoop /mnt/blobfusetmp

#geo
blobfuse ~/geo --tmp-path=/mnt/resource/blobfusetmp  --config-file=/home/mattcoop/.fuse_geo.cfg -o attr_timeout=240 -o entry_timeout=240 -o negative_timeout=120

#globeland30
blobfuse ~/globeland30 --tmp-path=/mnt/resource/blobfusetmp  --config-file=/home/mattcoop/.fuse_globeland30.cfg -o attr_timeout=240 -o entry_timeout=240 -o negative_timeout=120

#stan-models
blobfuse ~/stan-models --tmp-path=/mnt/resource/blobfusetmp  --config-file=/home/mattcoop/.fuse_stan-models.cfg -o attr_timeout=240 -o entry_timeout=240 -o negative_timeout=120

#dhsprocessed
blobfuse ~/dhsprocessed --tmp-path=/mnt/resource/blobfusetmp  --config-file=/home/mattcoop/.fuse_dhsprocessed.cfg -o attr_timeout=240 -o entry_timeout=240 -o negative_timeout=120

#dhsdata
blobfuse ~/dhsdata --tmp-path=/mnt/resource/blobfusetmp  --config-file=/home/mattcoop/.fuse_dhsdata.cfg -o attr_timeout=240 -o entry_timeout=240 -o negative_timeout=120

