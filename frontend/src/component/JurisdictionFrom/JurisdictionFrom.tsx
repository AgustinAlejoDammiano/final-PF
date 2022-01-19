import React, { useState } from "react";
import useStyles from "./styles";
import { Button, TextField, Typography } from "@material-ui/core";
import CircularProgress from '@material-ui/core/CircularProgress';
import JurisdictionRepository from "../../repository/JurisdictionRepository";

const repository = new JurisdictionRepository()

export default function JurisdictionFrom() {
    const classes = useStyles();

    const [name, setName] = useState<string | undefined>(undefined);
    const [loading, setLoading] = React.useState(false);

    const onSummit = () => {
        if (name !== undefined) {
            setLoading(true)
            repository.post(name)
                .then((r) => window.location.reload()) //TODO maybe add toast
                .catch((error: Error) => console.log(error))
                .finally(() => setLoading(false))
        }
    }

    return (
        <div className={classes.rowContainer}>
            <div className={classes.rowContainer}>
                <TextField className={classes.formControl} label={"Name"}
                    multiline rows={1} variant="outlined" onChange={(e) => setName(e.target.value as string)} />
            </div>

            <div className={classes.rowContainer} style={{ position: 'relative', }}>
                <Button className={classes.button} onClick={onSummit} disabled={loading || name === undefined}>
                    <Typography variant="h6">Create</Typography>
                </Button>
                {loading && <CircularProgress size={24} className={classes.buttonProgress} />}
            </div>
        </div >
    )
}
