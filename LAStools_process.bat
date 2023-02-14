:: a batch script for converting photogrammetry points into a
:: number of products with a tile-based multi-core batch pipeline
:: include LAStools in PATH to allow running script from anywhere
set PATH=%PATH%;C:\LAStools\bin
set LAStools=C:\LAStools\bin
::Tile size can be expanded in areas with lower point density areas
set TILE_SIZE=75
::Ensure buffer size is at least 1x if not 2x the step size
set BUFFER=20
::set cores to be n-1 on your processing machine
set CORES=11
::list of 
set list= 22_047_Fortress 22_081_Fortress 22_096 22_110_Fortress 22_118_CanRid 22_123_Fortress 22_144_Fortress 22_153_Fortress 22_179_Fortress 22_279_Fortress 22_292
set local_path= Z:\lidar-processing
set shp_name=FortRidgeSouth
set STEP=0.01
lastile -version
pause

FOR %%A IN (%list%) DO (
lasclip -i %local_path%\data\point_cloud\%%A.las -poly %local_path%\data\shp\%shp_name%.shp -o %local_path%\data\clipped\%%A_clip.las -v
pause
lasoptimize -i %local_path%\data\clipped\%%A_clip.las -o %local_path%\data\opt\%%A_opt.las -cpu64
:: create temp1orary tile directory
rmdir 1_tiles /s /q
mkdir 1_tiles
rem :: create a temp1orary tiling with tile size and buffer 30
lastile -i %local_path%\data\opt\%%A_opt.las ^
         -set_classification 0 ^
         -tile_size %TILE_SIZE% -buffer %BUFFER% -flag_as_withheld ^
         -o 1_tiles\tile.las
rmdir 2_tiles_denoised /s /q
mkdir 2_tiles_denoised
lasnoise -i 1_tiles\tile*.las ^
         -step 1 -isolated 25 ^
         -remove_noise ^
         -odir 2_tiles_denoised ^
         -cores %CORES%
rmdir 3_tiles_sorted /s /q
mkdir 3_tiles_sorted
lassort -i 1_tiles\*.las ^
        -odir 3_tiles_sorted -olas ^
        -cores %CORES%
rmdir 4_tiles_ground /s /q
mkdir 4_tiles_ground
lasground_new -i 3_tiles_sorted\tile*.las ^
              -step 3 ^
              -extra_fine ^
              -spike 0.5 ^
              -spike_down 2.5 ^
              -ground_class 2 ^
              -odir 4_tiles_ground ^
              -cores %CORES%
lasmerge -i 4_tiles_ground\tile*.las ^
         -drop_withheld ^
         -o %local_path%\data\class_points\%%A_class.las -olas
blast2dem -i %local_path%\data\class_points\%%A_class.las^
          -step %STEP% -utm 11U -keep_class 2 -o %local_path%\data\dsm\DSM_%%A.tif
rmdir 1_tiles /s /q
rmdir 2_tiles_denoised /s /q
rmdir 3_tiles_sorted /s /q
rmdir 4_tiles_ground /s /q
)
