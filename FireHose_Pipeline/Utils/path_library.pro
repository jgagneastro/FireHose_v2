Function path_library, marker, HOSTNAME=hostname, SOFT=soft
;  This function takes a marker in entry, then returns the path corresponding to this marker, 
;    depending on which machine IDL is now running.
;  To get the exact name of the current machine, just type "spawn, 'hostname', hostname & print, hostname"
;    in the IDL console. Then you can start adding markers with their paths.
  
  compile_opt hidden
  on_error, 2
  
  ;Check on which machine this code is running
  if ~keyword_set(hostname) then $
    spawn, 'hostname', hostname
  hostname = strlowcase(hostname)
  
  ;Find the path corresponding to the marker
  case strlowcase(marker) of
    'loci_calibration' : $
      begin
        case hostname of
          'eris' :     p = [$
                            'C:\Users\JONATHAN\Documents\Univ\Doctorat\LOCI\Calibration\' $
                           ,'C:\Users\JONATHAN\Documents\My Dropbox\Locicalib\' $
                           ]
          'varuna' :   p = [$
                            'C:\Users\Administrator\Documents\Univ\Doctorat\LOCI\Calibration\' $
                           ,'C:\Users\Administrator\Documents\My Dropbox\Dropbox\Locicalib\' $
                           ]
          'makara' :   p = [$
                            'C:\Users\Administrator\Documents\Univ\Doctorat\LOCI\Calibration\' $
                           ,'C:\Users\Administrator\Documents\My Dropbox\Dropbox\Locicalib\' $
                           ]
          'genesis' :  p = [$
                            '/home/cpapir/Jonathan/calibration_loci/' $
                           ,'' $
                           ]
          else : goto, badhost 
        endcase
      end ;loci_calibration
    'loci_setup' : $
      begin
        case hostname of
          'eris' :     p = [$
                            'D:\Donnees\LOCI\' $
                           ,'C:\Users\JONATHAN\Documents\Univ\Doctorat\LOCI\' $
                           ]
          'varuna' :   p = [$
                            'D:\Donnees\LOCI\' $
                           ,'C:\Users\Administrator\Documents\Univ\Doctorat\LOCI\' $
                           ]
          'makara' :   p = [$
                            'D:\Donnees\LOCI\' $
                           ,'C:\Users\Administrator\Documents\Univ\Doctorat\LOCI\' $
                           ]
          else : goto, badhost 
        endcase
      end ;loci_setup
    'email' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Program Files (x86)\sendEmail-v156\sendEmail.exe' $
                           ,'C:\Users\JONATHAN\Documents\Univ\Doctorat\IDL\']
          'varuna' :   p = ['C:\Program Files (x86)\sendEmail-v156\sendEmail.exe' $
                           ,'C:\Users\Administrator\Documents\Univ\Doctorat\IDL\']
          'makara' :   p = ['C:\Program Files (x86)\sendEmail-v156\sendEmail.exe' $
                           ,'C:\Users\Administrator\Documents\Univ\Doctorat\IDL\']
          else : goto, badhost 
        endcase
      end ;email
     'tmp' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\IDL\tmp\']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\IDL\tmp\']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\IDL\tmp\']
          'genesis' :  p = ['/home/gagne/idl/tmp/']
          'arcturus' : p = '/Users/gagne/Documents/IDL/tmp/'
          else : goto, badhost 
        endcase
      end ;tmp
     '2mass_data' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\2MASS\']
          'varuna' :   p = ['C:\Donnees\2MASS\']
          'makara' :   p = ['C:\Donnees\2MASS\']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/2MASS/']
          else : goto, badhost 
        endcase
      end ;2MASS_data
      '2mass_fits' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\2MASS\2MASS_PSC_fits\']
          'varuna' :     p = ['C:\Donnees\2MASS\2MASS_PSC_fits\']
          'makara' :     p = ['C:\Donnees\2MASS\2MASS_PSC_fits\']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/2MASS/2MASS_PSC_fits/']
          else : goto, badhost 
        endcase
      end ;2MASS_fits
      'default' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\']
          'varuna' :   p = ['C:\Users\Administrator\']
          'makara' :   p = ['C:\Users\Administrator\']
          'arcturus' : p = ['/Users/gagne/']
          else : goto, badhost 
        endcase
      end ;default
      'mstars' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\M stars\']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/M stars/']
          else : goto, badhost 
        endcase
      end ;Mstars
      'mstruc' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\M stars\data_structures\']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\data_structures\']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\data_structures\']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/M stars/data_structures/']
          else : goto, badhost 
        endcase
      end ;Mstruc
      'mcsv' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\M stars\data_csv\']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\data_csv\']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\data_csv\']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/M stars/data_csv/']
          else : goto, badhost 
        endcase
      end ;Mcsv
      'dwarfarchive' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\IDL\BDarchive\BDarchive.txt']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\IDL\BDarchive\BDarchive.txt']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\IDL\BDarchive\BDarchive.txt']
          'genesis' :  p = ['/home/cpapir/logfiles/BDarchive/BDarchive']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/IDL/BDarchive/BDarchive.txt']
          else : goto, badhost 
        endcase
      end ;dwarfarchive
      'wisedata' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\WISE\Catalogs\']
          'varuna' :   p = ['C:\Donnees\WISE\Catalogs\']
          'makara' :   p = ['C:\Donnees\WISE\Catalogs\']
          'genesis' :  p = ['/home/cpapir/wise/catalogs/']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/WISE/Catalogs']
          else : goto, badhost 
        endcase
      end ;wisedata
      'wisecat' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\WISE\Catalog\']
          'varuna' :   p = ['C:\Donnees\WISE\Catalog\']
          'makara' :   p = ['C:\Donnees\WISE\Catalog\']
          'genesis' :  p = ['/home/cpapir/wise/catalog/']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/WISE/OUT_LIGHT/']
          else : goto, badhost 
        endcase
      end ;wisecat
      'tmasscat' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\2MASS\Catalog\']
          'varuna' :   p = ['C:\Donnees\2MASS\Catalog\']
          'makara' :   p = ['C:\Donnees\2MASS\Catalog\']
          'genesis' :  p = ['/home/cpapir/archives/2mass/catalog/']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/2MASS/Catalog_Portable/']
          else : goto, badhost 
        endcase
      end ;tmasscat
      'data' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\']
          'varuna' :   p = ['C:\Donnees\']
          'makara' :   p = ['C:\Donnees\']
          'genesis' :   p = ['/home/cpapir/data/']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/']
          else : goto, badhost 
        endcase
      end ;data
      'cpapirdata' : $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\CPAPIR\']
          'varuna' :   p = ['C:\Donnees\CPAPIR\']
          'makara' :   p = ['C:\Donnees\CPAPIR\']
          'genesis' :   p = ['/home/cpapir/data/']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/CPAPIR/']
          else : goto, badhost 
        endcase
      end ;data
      'ssh' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\IDL\']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\IDL\']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\IDL\']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/IDL/']
          else : goto, badhost 
        endcase
      end ;SSH
      'winscp' : $
      begin
        case hostname of
          'eris' :       p = ['C:\Program Files (x86)\WinSCP\winscp.exe']
          'varuna' :     p = ['C:\Program Files (x86)\WinSCP\winscp.exe']
          'makara' :     p = ['C:\Program Files (x86)\WinSCP\winscp.exe']
          else : goto, badhost 
        endcase
      end ;winscp
      'workspace' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\IDL_workspace3\']
          'arcturus' : p = ['/Users/gagne/Documents/IDL/IDL_Library/']
          else : goto, badhost 
        endcase
      end ;workspace
      'backup' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\IDL_backups\']
          'arcturus' : p = ['/Users/gagne/Documents/IDL/IDL_Backups/']
          else : goto, badhost 
        endcase
      end ;backup
      'desktop' : $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Desktop\']
          'arcturus' : p = ['/Users/gagne/Documents/Workspace/']
          else : goto, badhost 
        endcase
      end ;desktop
      'logfiles': $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Logs\']
          'arcturus' : p = ['/Users/gagne/Documents/Workspace/Logs/']
          else : goto, badhost 
        endcase
      end ;logfiles
      'cpapir_data': $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\CPAPIR\']
          'varuna' :     p = ['C:\Donnees\CPAPIR\']
          'makara' :     p = ['C:\Donnees\CPAPIR\']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/CPAPIR/']
          else : goto, badhost 
        endcase
      end ;cpapir_data
      'opiomm': $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\Opiomm\']
          'varuna' :     p = ['C:\Users\Administrator\Documents\Univ\Doctorat\Opiomm\']
          'makara' :     p = ['C:\Users\Administrator\Documents\Univ\Doctorat\Opiomm\']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/OPIOMM/']
          else : goto, badhost 
        endcase
      end ;opiomm
      'firefox': $
      begin
        case hostname of
          'eris' : p = ['C:\Program Files (x86)\Mozilla Firefox\firefox.exe']
          'arcturus' : p = ['/Applications/Firefox.app']
          else : goto, badhost
        endcase
      end ;firefox
      'chrome': $
      begin
        case hostname of
          'arcturus' : p = ['/Applications/Google Chrome.app']
          else : goto, badhost
        endcase
      end ;chrome
      'ds9': $
      begin
        case hostname of
          'eris' : p = ['C:\Program Files\ds9\ds9.exe']
          'genesis' : p = ['ds9']
          'arcturus' :p = ['/Applications/ds9_dir/ds9.darwinlion.7.0.2/ds9']
          else : goto, badhost
        endcase
      end ;ds9
      'obsolete_pro': $
      begin
        case hostname of
          'eris' : p = ['C:\Users\JONATHAN\Documents\IDL_workspace3\00 - Librairie\00 - Jo\Obsoletes\']
          'varuna' : p = ['C:\Users\Administrator\Documents\IDL_workspace3\00 - Librairie\00 - Jo\Obsoletes\']
          'makara' : p = ['C:\Users\Administrator\Documents\IDL_workspace3\00 - Librairie\00 - Jo\Obsoletes\']
          'arcturus' : p = ['/Users/gagne/Documents/IDL/IDL_Library/00 - Librairie/00 - Jo/Obsoletes/']
          else : goto, badhost
        endcase
      end ;obsolete_pro
      'idl': $
      begin
        case hostname of
          'eris' : p = ['C:\Program Files\ITT\IDL\IDL81\bin\bin.x86_64\idl.exe']
          'arcturus' : p = ['/Applications/itt/idl/idl81/idlde/idlde.darwin.x86_64.app']
          else : goto, badhost
        endcase
      end ;idl
      'correl_2mass_wise': $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\M stars\mega_correl\']
          'varuna' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\mega_correl\']
          'makara' :   p = ['C:\Users\Administrator\Documents\Univ\Doctorat\M stars\mega_correl\']
          'genesis' :   p = ['/home/cpapir/archives/wise_2mass/']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/M stars/mega_correl_mars/']
          else : goto, badhost
        endcase
      end ;correl_2mass_wise
      'correl_2mass_wise_allsky': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/M stars/mega_correl_allsky/']
          else : goto, badhost
        endcase
      end ;correl_2mass_wise_allsky
      'astrometrynet': $
      begin
        case hostname of
          'eris' :     p = ['/cygdrive/c/cygwin/usr/local/astrometry/bin/']
          else : goto, badhost
        endcase
      end ;astrometrynet
      'sav_files': $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Doctorat\IDL\sav_files\']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Doctorat/IDL/sav_files/']
          else : goto, badhost
        endcase
      end ;sav_files
      'sav': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/IDL/save/']
          else : goto, badhost
        endcase
      end ;sav
      '7zip': $
      begin
        case hostname of
          'eris' :     p = ['C:\Program Files (x86)\7-Zip\7z.exe']
          else : goto, badhost
        endcase
      end ;7zip
      'passbands': $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\Passbands\']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/Passbands/']
          else : goto, badhost
        endcase
      end ;passbands
      'solvefield_dir': $
      begin
        case hostname of
          'genesis' :     p = ['/usr/local/astrometry/bin/']
          else : goto, badhost
        endcase
      end ;solvefield_dir
      'models': $
      begin
        case hostname of
          'eris' :     p = ['D:\Donnees\Stellar_Dwarf_Models\']
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/Stellar_Dwarf_Models/']
          else : goto, badhost
        endcase
      end ;models
      '6771': $
      begin
        case hostname of
          'eris' :     p = ['C:\Users\JONATHAN\Documents\Univ\Session 10 Hi 12\PHY 6771 ; Atmospheres Stellaires\']
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Session 10 Hi 12/PHY 6771 ; Atmospheres Stellaires/']
          else : goto, badhost
        endcase
      end ;models
      '643': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Session 10 Hi 12/PHYS 643 ; Astrophysical Fluids/']
          else : goto, badhost
        endcase
      end ;astrophysical fluids
      'assoc': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/Workspace/Associations/']
          else : goto, badhost
        endcase
      end ;Assoc
      'members': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/Workspace/Associations/Membres/']
          else : goto, badhost
        endcase
      end ;Members
      'marieeve': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/Univ/Session 10 Hi 12/PHY 6771 ; Atmospheres Stellaires/Devoir Numerique 4/MarieEve/']
          else : goto, badhost
        endcase
      end;Devoir Marie-Eve
      'pluto': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Library/Scripts/PLUTO/']
          else : goto, badhost
        endcase
      end;PLUTO
      'plx_txt': $
      begin
        p = [path_library('data')+'PLX'+path_sep()+'plx_dupuy.txt']
      end;plx_text
      'plx': $
      begin
        p = [path_library('data')+'PLX'+path_sep()+'plx_dupuy.sav']
      end;plx
      'plxcat': $
      begin
        p = [path_library('data')+'PLX'+path_sep()+'plx_catalog.sav']
      end;plxcat
      'dwl': $
      begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Downloads/']
          else : goto, badhost
        endcase
      end;DWL
      'mearth': $
        begin
        case hostname of
          'arcturus' : p = ['/Users/gagne/Documents/Donnees/PLX/Dittmann2013_Mearth.sav']
          else : goto, badhost
        endcase
      end;DWL
      'vega': $
      begin
        p = [path_library('data')+'Models'+path_sep()+'Vega_Spectrum.txt']
      end;VEGA
      'vrad': $
      begin
        p = [path_library('data')+'VRAD'+path_sep()+'vrad_catalog.sav']
      end;VRAD
      else : if keyword_set(soft) then return, '' else message, 'The marker "'+strtrim(marker,2)+'" couldn''t be recognized.'
  endcase
  
  if n_elements(p) eq 1 then return, p[0]
  return, p
  
  BADHOST :
  if keyword_set(soft) then return, ''
  message, 'The machine "'+strtrim(hostname,2)+'" is not associated with a "'+strtrim(marker,2)+'" marker.'
End