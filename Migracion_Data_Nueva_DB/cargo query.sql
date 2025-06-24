SET DATEFORMAT YMD;
SET NOCOUNT ON;
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

DECLARE
   @ErrMsg        NVARCHAR(4000),
   @ErrorSeverity INT,
   @ErrorState    INT,
   @ErrorNumber   INT,
   @ErrorLine     INT,
   @NUMEROOPI     VARCHAR(20),
   @NROUNICO      INT = 0,
   @NROUNICOLOT   INT = 0,
   @FechaRegistro  DATETIME = GETDATE(),  
   @TotalMonto     DECIMAL(18, 2) = 0,       
   @TotalItem      DECIMAL(18, 2),        
   @FechaConvertida VARCHAR(23),  
   @NroLinea       INT = 1;  -- Inicializar el contador de líneas

SET @FechaConvertida = CONVERT(VARCHAR, @FechaRegistro, 120);  -- Convierte la fecha

BEGIN TRANSACTION;
BEGIN TRY
  -- Cursor para obtener los diferentes CodUbic
  DECLARE @CodUbic NVARCHAR(10);
  DECLARE curUbic CURSOR FOR
  SELECT DISTINCT CodUbic FROM SEASecundariaDB.dbo.SAEXIS WHERE Existen > 0 ORDER BY CodUbic;

  OPEN curUbic;
  FETCH NEXT FROM curUbic INTO @CodUbic;

  WHILE @@FETCH_STATUS = 0
  BEGIN
    -- Reiniciar variables para cada CodUbic
    SET @TotalMonto = 0;
    SET @NroLinea = 1;

    EXEC SP_ADM_PROXCORREL '00000', '', 'PrxCargo', @NUMEROOPI OUTPUT;
    
    IF NOT EXISTS(SELECT NUMEROD FROM SEA_Abir2025DB.dbo.SAOPEI WITH (NOLOCK)
                   WHERE CODSUCU = '00000' AND
                         TIPOOPI = 'O' AND NUMEROD = @NUMEROOPI)
    BEGIN
      UPDATE SEA_Abir2025DB.dbo.SAOPEI 
      SET [TipoOpI] = 'O',
          [NumeroD] = @NUMEROOPI,
          [Signo] = 1,
          [CodEsta] = 'SERVER',
          [CodUsua] = '001',
          [Autori] = 'SISTEMA -CARGO INICIAL HOMOLOGACION',
          [Respon] = 'SISTEMAS - GERENCIA',
          [UsoMat] = 'CARGO INICIAL DE INVENTARIO HOMOLOGACION',
          [CodUbic] = @CodUbic,
          [Monto] = @TotalMonto, 
          [FechaE] = @FechaRegistro,  
          [FechaT] = @FechaRegistro,  
          [CodOper] = '',
          [UsoInterno] = 0
      WHERE (CodSucu = '00000') AND (TipoOpI = 'O') AND (NumeroD = @NUMEROOPI);
      
      IF @@ROWCOUNT = 0
      INSERT INTO SEA_Abir2025DB.dbo.SAOPEI ([TipoOpI], [NumeroD], [Signo], [CodSucu], [CodEsta], [CodUsua], [Autori], [Respon], [UsoMat], [CodUbic], [Monto], [FechaE], [FechaT])
      VALUES ('O', @NUMEROOPI, 1, '00000', 'DESARROLLO', '001', 'SERVER CARGO INICIAL HOMOLOGACION', 'SISTEMAS - GERENCIA', 'CARGO INICIAL DE INVENTARIO HOMOLOGACION', @CodUbic, @TotalMonto, @FechaRegistro, @FechaRegistro);
      
      -- Actualizar SAPROD y ejecutar TR_ADM_UPDATE_EXISTENCIAS para cada producto
      DECLARE @CodProd NVARCHAR(10), @Cantidad DECIMAL(18, 2), @Costo DECIMAL(18, 2),
              @Descrip1 NVARCHAR(60), @Descrip2 NVARCHAR(60);
      
      DECLARE cur CURSOR FOR
      SELECT Pro.CodProd, Exi.Existen, Pro.Precio1, Pro.Descrip, Pro.Descrip2
      FROM SEASecundariaDB.dbo.SAEXIS Exi 
      INNER JOIN SEASecundariaDB.dbo.SAPROD Pro ON Exi.CodProd = Pro.CodProd
      WHERE Exi.Existen > 0 AND Exi.CodUbic = @CodUbic;  -- Filtrar por CodUbic

      OPEN cur;
      FETCH NEXT FROM cur INTO @CodProd, @Cantidad, @Costo, @Descrip1, @Descrip2;

      WHILE @@FETCH_STATUS = 0
      BEGIN
          SET @TotalItem = @Cantidad * @Costo;
          SET @TotalMonto = @TotalMonto + @TotalItem;  -- Sumar al monto total

          -- Actualizar el costo del producto
          UPDATE SEA_Abir2025DB.dbo.SAPROD 
          SET CostPro = CASE WHEN Existen + @Cantidad > 0 THEN ((CostPro * Existen) + (@TotalItem)) / (Existen + @Cantidad) ELSE 100.00 END
          WHERE CodProd = @CodProd;

          EXEC TR_ADM_UPDATE_EXISTENCIAS '00000', @CodProd, @CodUbic, @Cantidad, 0, @FechaConvertida; 

          -- Insertar en SAITEMOPI
          INSERT INTO SEA_Abir2025DB.dbo.SAITEMOPI ([CodSucu], [TipoOpI], [NumeroD], [NroLinea], [FechaE], [CodItem], [CodUbic], [Signo], [Descrip1],[Descrip2] , [Cantidad], 
              [Costo], [NroUnicoL], [TotalItem], [FechaL])
          VALUES ('00000', 'O', @NUMEROOPI, @NroLinea, @FechaRegistro, @CodProd, @CodUbic, 1, @Descrip1, @Descrip2, @Cantidad, @Costo, ISNULL(@NROUNICOLOT, 0), @TotalItem, @FechaRegistro);  -- Usar la variable de fecha

          SET @NroLinea = @NroLinea + 1;  -- Incrementar el contador de líneas
          FETCH NEXT FROM cur INTO @CodProd, @Cantidad, @Costo, @Descrip1, @Descrip2;  -- Asegúrate de incluir todas las variables
      END

      CLOSE cur;
      DEALLOCATE cur;
    END
    ELSE
    BEGIN
       SET @ErrMsg = 'El número del correlativo ya existe.';
       RAISERROR(@ErrMsg, 16, 0);
    END;

    FETCH NEXT FROM curUbic INTO @CodUbic;  -- Obtener el siguiente CodUbic
  END

  CLOSE curUbic;
  DEALLOCATE curUbic;

  COMMIT TRANSACTION;
  SELECT 0 AS error, ISNULL(@NUMEROOPI, '') AS numerod;
END TRY
BEGIN CATCH
  IF (@@TRANCOUNT > 0)
     ROLLBACK;
  SELECT
     @ErrMsg = ERROR_MESSAGE(),
     @ErrorSeverity = ERROR_SEVERITY(),
     @ErrorState = ERROR_STATE(),
     @ErrorNumber = ERROR_NUMBER(),
     @ErrorLine = ERROR_LINE();
  SET @ErrMsg = @ErrMsg + CHAR(13) + 'Line: ' + CAST(@ErrorLine AS VARCHAR(10));
  SELECT -1 AS error, @ErrMsg AS errmsg, @ErrorSeverity AS errseverity;
  RAISERROR(@ErrMsg, @ErrorSeverity, @ErrorState);
END CATCH;


-- SELECT * FROM SAEXIS