      module ProSymModule
         TYPE DES_FILE_CONTENT
            SEQUENCE
            CHARACTER (LEN=50) :: TITLE
            CHARACTER (LEN=20) :: DEFAULT_VALUE            
            CHARACTER (LEN=5)  :: TYPE            
            INTEGER (KIND=2)   :: LENGTH 
            INTEGER (KIND=2)   :: POS
            CHARACTER (LEN=256) :: LIST
!            CHARACTER (LEN=256) :: HELP_MSG
         END TYPE DES_FILE_CONTENT  
         TYPE RESOURCE_LIST
            SEQUENCE
            CHARACTER (LEN=50) :: RESOURCE_NAME
            CHARACTER (LEN=6)  :: ASSET_CLASS_ID
            CHARACTER (LEN=1)  :: ACTIVE            
         END TYPE RESOURCE_LIST  
         Interface
            SUBROUTINE ProSymInputData(Process_Base_File,OverlayName)
               LOGICAL (KIND=4) :: Process_Base_File
               CHARACTER (LEN=*), OPTIONAL :: OverlayName
            END SUBROUTINE ProSymInputData
         End Interface
      end module ProSymModule