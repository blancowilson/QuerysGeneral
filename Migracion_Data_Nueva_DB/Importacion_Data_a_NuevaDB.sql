-- clientes
insert into SEA_Abir2025DB.dbo.saclie ([CodClie], [Descrip], [ID3], [TipoID3], [TipoID], [Activo], [DescOrder], [Clase], [Represent], [Direc1], [Direc2], [Pais], [Estado], [Ciudad], [Municipio], [ZipCode], [Telef], [Movil], [Email], [Fax], [FechaE], [CodZona], [CodVend], [CodConv], [CodAlte], [TipoCli], [TipoReg], [TipoPVP], [Observa], [EsMoneda], [EsCredito], [LimiteCred], [DiasCred], [EsToleran], [DiasTole], [IntMora], [Descto], [Saldo], [PagosA], [FechaUV], [MontoUV], [NumeroUV], [FechaUP], [MontoUP], [NumeroUP], [MontoMax], [MtoMaxCred], [PromPago], [RetenIVA], [SaldoPtos], [EsReten], [DescripExt])
select  [CodClie], [Descrip], [ID3], [TipoID3], [TipoID], [Activo], [DescOrder], [Clase], [Represent], [Direc1], [Direc2], [Pais], [Estado], [Ciudad], [Municipio], [ZipCode], [Telef], [Movil], [Email], [Fax], [FechaE], [CodZona], [CodVend], [CodConv], [CodAlte], [TipoCli], [TipoReg], [TipoPVP], [Observa], [EsMoneda], [EsCredito], [LimiteCred], [DiasCred], [EsToleran], [DiasTole], [IntMora], [Descto], [Saldo], [PagosA], [FechaUV], [MontoUV], [NumeroUV], [FechaUP], [MontoUP], [NumeroUP], [MontoMax], [MtoMaxCred], [PromPago], [RetenIVA], [SaldoPtos], [EsReten], [DescripExt]
From SEASecundariaDB.dbo.SACLIE
where CodClie not in (select CodClie from SEA_Abir2025DB.dbo.saclie)

--Proveedores
insert into SEA_Abir2025DB.dbo.SAPROV ([CodProv], [Descrip], [TipoPrv], [TipoID3], [TipoID], [ID3], [DescOrder], [Clase], [Activo], [Represent], [Direc1], [Direc2], [Pais], [Estado], [Ciudad], [Municipio], [ZipCode], [Telef], [Movil], [Fax], [Email], [FechaE], [EsReten], [RetenISLR], [DiasCred], [Observa], [EsMoneda], [Saldo], [MontoMax], [PagosA], [PromPago], [RetenIVA], [FechaUC], [MontoUC], [NumeroUC], [FechaUP], [MontoUP], [NumeroUP])
select [CodProv], [Descrip], [TipoPrv], [TipoID3], [TipoID], [ID3], [DescOrder], [Clase], [Activo], [Represent], [Direc1], [Direc2], [Pais], [Estado], [Ciudad], [Municipio], [ZipCode], [Telef], [Movil], [Fax], [Email], [FechaE], [EsReten], [RetenISLR], [DiasCred], [Observa], [EsMoneda], [Saldo], [MontoMax], [PagosA], [PromPago], [RetenIVA], [FechaUC], [MontoUC], [NumeroUC], [FechaUP], [MontoUP], [NumeroUP]
from SEASecundariaDB.dbo.SAPROV
where Codprov not in (select CodProv from SEA_Abir2025DB.dbo.SAPROV)


BEGIN TRY
    BEGIN TRANSACTION;
	-- Activar inserción manual de valores en la columna identity
	SET IDENTITY_INSERT  [SEA_Abir2025DB].[dbo].[SAINSTA] ON;

	INSERT INTO [SEA_Abir2025DB].[dbo].[SAINSTA] ( [CodInst], [InsPadre], [Nivel], [TipoIns], [Descrip], [Descto], [DEsComp],
			[DEsSeri], [DEsLote], [DEsComi],  [DEsCorrel],  [DigitosC],  [DEsTabla], [CodAlte])
	SELECT 
		[CodInst], [InsPadre], [Nivel], [TipoIns],  [Descrip], [Descto], [DEsComp], [DEsSeri], [DEsLote], [DEsComi], [DEsCorrel], [DigitosC],
		[DEsTabla],    [CodAlte]
	FROM SEASecundariaDB.[dbo].[SAINSTA]  -- Ajusta el nombre de la base de datos origen
	WHERE CODINST NOT IN (SELECT CODINST FROM [SEA_Abir2025DB].[dbo].[SAINSTA]);

		SET IDENTITY_INSERT [SEA_Abir2025DB].[dbo].[SAINSTA] OFF;

		-- Resetear identity al máximo actual
		DECLARE @maxCodInst INT;
		SELECT @maxCodInst = MAX([CodInst]) FROM [SEA_Abir2025DB].[dbo].[SAINSTA];
		IF @maxCodInst IS NULL SET @maxCodInst = 0;  -- Si la tabla estaba vacía
		DBCC CHECKIDENT ('[SEA_Abir2025DB].[dbo].[SAINSTA]', RESEED, @maxCodInst);

	COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;
    THROW;
END CATCH

insert into SEA_Abir2025DB.dbo.SADEPO ([CodUbic], [Descrip], [Clase], [Activo], [Represent], [Direc1], [Direc2], [ZipCode], [Telef], [Printer])
select [CodUbic], [Descrip], [Clase], [Activo], [Represent], [Direc1], [Direc2], [ZipCode], [Telef], [Printer]
from SEASecundariaDB.[dbo].SADEPO
WHERE CodUbic NOT IN (SELECT CodUbic FROM SEA_Abir2025DB.dbo.SADEPO)

insert into SEA_Abir2025DB.dbo.SAPROD ([CodProd], [Descrip], [CodInst], [Activo], [Descrip2], [Descrip3], [Refere], [Marca], [Unidad], [UndEmpaq], [CantEmpaq], [Precio1], [PrecioI1], [PrecioIU1], [Precio2], [PrecioU2], [PrecioI2], [PrecioIU2], [Precio3], [PrecioU3], [PrecioI3], [PrecioIU3], [PrecioU], [CostAct], [CostPro], [CostAnt], [Existen], [ExUnidad], [Compro], [Pedido], [Minimo], [Maximo], [Tara], [Factor], [DEsComp], [DEsComi], [DEsSeri], [EsReten], [DEsLote], [DEsVence], [EsImport], [EsExento], [EsEnser], [EsOferta], [EsPesa], [EsEmpaque], [ExDecimal], [DiasEntr], [FechaUV], [FechaUC], [DiasTole], [Peso], [Volumen], [UndVol])
select [CodProd], [Descrip], [CodInst], [Activo], [Descrip2], [Descrip3], [Refere], [Marca], [Unidad], [UndEmpaq], [CantEmpaq], [Precio1], [PrecioI1], [PrecioIU1], [Precio2], [PrecioU2], [PrecioI2], [PrecioIU2], [Precio3], [PrecioU3], [PrecioI3], [PrecioIU3], [PrecioU], [CostAct], [CostPro], [CostAnt], 0 EXISTEN, [ExUnidad], [Compro], [Pedido], [Minimo], [Maximo], [Tara], [Factor], [DEsComp], [DEsComi], [DEsSeri], [EsReten], [DEsLote], [DEsVence], [EsImport], [EsExento], [EsEnser], [EsOferta], [EsPesa], [EsEmpaque], [ExDecimal], [DiasEntr], [FechaUV], [FechaUC], [DiasTole], [Peso], [Volumen], [UndVol]
from SEASecundariaDB.[dbo].SAPROD
WHERE CodProd NOT IN (SELECT CodProd FROM SEA_Abir2025DB.dbo.SAPROD)

INSERT INTO SEA_Abir2025DB.dbo.sacodbar (CodProd, CodAlte)
SELECT CODPROD, CODPROD 
FROM  SEA_Abir2025DB.dbo.SAPROD 
WHERE CODPROD NOT IN (SELECT CODPROD FROM  SEA_Abir2025DB.dbo.sacodbar)



-- Usuarios
MERGE INTO SEA_Abir2025DB.dbo.SSUSRS AS destino
USING SEASecundariaDB.dbo.SSUSRS AS origen
ON destino.CodUsua = origen.CodUsua  -- Cambia 'CodUsua' por la clave primaria o el campo que identifique de manera única los registros
WHEN MATCHED THEN
    UPDATE SET
        destino.Descrip = origen.Descrip,
        destino.Email = origen.Email,
        destino.Movil = origen.Movil,
        destino.Level = origen.Level,
        destino.Activo = origen.Activo,
        destino.Password = origen.Password,
        destino.[IdUserNot] = origen.[IdUserNot], 
        destino.Profile = origen.Profile,
        destino.CodVend = origen.CodVend,
        destino.SData1 = origen.SData1,
        destino.SData2 = origen.SData2,
        destino.SData3 = origen.SData3
WHEN NOT MATCHED THEN
    INSERT (CodUsua, Descrip, Email, Movil, Level, Activo, Password, [IdUserNot], Profile, CodVend, SData1, SData2, SData3)
    VALUES (origen.CodUsua, origen.Descrip, origen.Email, origen.Movil, origen.Level, origen.Activo, origen.Password, origen.[IdUserNot], origen.Profile, origen.CodVend, origen.SData1, origen.SData2, origen.SData3);

    -- CORRELATIVOS
    WITH CORRELSIS_ORIGEN AS (
SELECT [CodSucu], [FieldName], [CodEsta], [Prefijo], [ValueInt], [Desde], [Hasta], [LenCorrel]
from SEASecundariaDB.dbo.SACORRELSIS ) 
UPDATE DESTINO 
SET  DESTINO.[Prefijo]=ORIGEN.[Prefijo],DESTINO.[ValueInt]= ORIGEN.[ValueInt], DESTINO.[Desde]= ORIGEN.[Desde], DESTINO.[Hasta]= ORIGEN.[Hasta], 
    DESTINO.[LenCorrel] = ORIGEN.[LenCorrel]
FROM SEA_Abir2025DB.dbo.SACORRELSIS DESTINO  
    INNER JOIN CORRELSIS_ORIGEN ORIGEN ON ORIGEN.FIELDNAME = DESTINO.FIELDNAME

-- SAEXiS sin Inventario
insert into SEA_Abir2025DB.dbo.SAEXIS( [CodSucu], [CodProd], [CodUbic], [PuestoI], [Existen], [ExUnidad], [CantPed], [UnidPed], [CantCom], [UnidCom])
select  [CodSucu], [CodProd], [CodUbic], [PuestoI], 0 EXISTEN,0 [ExUnidad],0 [CantPed],0 [UnidPed],0 [CantCom], 0 [UnidCom]
from SEASecundariaDB.[dbo].SAEXIS ORIGEN
WHERE NOT EXISTS (SELECT CodProd FROM SEA_Abir2025DB.dbo.SAEXIS DESTINO
                        WHERE ORIGEN.CodProd = DESTINO.CodProd)





--TODO mejorar agrupar por codubic 
--TODO MEJOJRAR CANTIDAD DE CEROS EN CORRELATIVO
insert into SEA_Abir2025DB.dbo.SAITEMOPI (TipoOpI, NumeroD, NroLinea, CodItem, Descrip1, CodUbic, Cantidad, Costo,Precio, FechaE, DEsSeri,CodSucu)
SELECT 'R','00000049', (ROW_NUMBER() OVER (ORDER BY Pro.codprod))+1 as Num,EXi.CodProd, Pro.descrip, '001',( Exi.Existen),(Exi.Existen)*Pro.Precio1,Precio1 ,getdate(),0,'00000'
from SEASecundariaDB.dbo.SAEXIS Exi inner join SEASecundariaDB.dbo.SAPROD Pro  ON Exi.CodProd = Pro.CodProd
where Exi.Existen>0 

select *
from SATARJ




--select *
--from SSFMTS

--select *
--from SSOPMN

--select *
--from SSPARD

--select *
--from SSPARM

